#!/usr/bin/ruby

require 'pg'
require 'open3'

db = PG.connect :dbname => 'ceqrobot'

prog = ARGV[0].upcase

res = db.exec <<-eof
  select
  distinct on (c.code)
    c.code,
    c.name,
    c.credits,
    c.level,
    cr.type,
    cr.programmeyear,
    q.satisfaction as value
  from course c
     , course_relation cr
     , ceq q
  where c.code = cr.code
  and q.code = c.code
  and q.year = 2015
  and cr.programme = '#{prog}'
  and cr.validyear = 2015
  union all values ('XXX000', 'Exjobb', 30.0, 'LevelA', 'Obligatory', 5, 0)
eof

description = res.map{ |r|
  "Y#{r['programmeyear']}. #{r['code']} - #{r['name']} " +
  "#{r['credits']} hp (#{r['level'].gsub(/Level/, '')})"
}

credits = res.map{ |r| r['credits'].to_f }
year = res.map{ |r| r['programmeyear'].to_i }
value = res.map{ |r| r['value'].to_i }

File.open('data.dzn', 'w') do |f|
  f.write "ncourses = #{res.count};\n"

  f.write 'bhcredits = [ '
  res.each do |r|
    if r['level'] != 'LevelA'
      f.write "#{(r['credits'].to_f * 2).to_i}, "
    else
      f.write "#{0}, "
    end
  end
  f.write ' ];'

  f.write 'ahcredits = [ '
  res.each do |r|
    if r['level'] == 'LevelA'
      f.write "#{(r['credits'].to_f * 2).to_i}, "
    else
      f.write "#{0}, "
    end
  end
  f.write ' ];'

  f.write 'obligatory = [ '
  res.each do |r|
    f.write "#{r['type'] == 'Obligatory'? 1 : 0}, "
  end
  f.write ' ];'

  f.write 'value = [ '
  res.each do |r|
    f.write "#{r['value'].to_i}, "
  end
  f.write ' ];'
end

iteration = 1

Open3.popen2('minizinc', '-a', '-d', 'data.dzn', 'diploma.mzn') do |stdin, stdout, status_thread|
  stdout.each_line do |line|
    next if /^-+$/.match? line

    puts "ITTERATION #{iteration}.\n"
    iteration += 1

    line = line.split(/[{}]/)[1]
    selected = line.split(',').map(&:to_i)

    totcred = 0.0
    totvalue = 0.0

    selected = selected.map{ |s| s - 1 }

    selected.sort_by{ |s| year[s] }.each do |s|
      puts description[s]
      totcred += credits[s]
      totvalue += value[s]
    end

    puts ""
    puts "#{totcred} hp"
    puts "Värde: #{totvalue.round(4)}"
    puts ""
  end
end
