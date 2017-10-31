import React from 'react'

import CourseRow from './CourseRow'
import CourseHeader from './CourseHeader'

export default class CourseTable extends React.Component {
  constructor(props) {
    super(props)

    let uniqueCourses = {}

    db.programmes.F.forEach(r => { uniqueCourses[r.code] = r })

    let cs = Object.values(uniqueCourses).map(dataForCourse)

    this.state = { courses: cs}
    this.handleSort = this.handleSort.bind(this)
  }

  handleSort(k, e) {
    const ceqProps = [ 'ceqPeriod'
                     , 'satisfaction'
                     , 'relevance'
                     , 'quality'
                     , 'workload'
                     , 'registered'
                     , 'passed'
                     , 'percentPassed'
                     ]
        , revProps = [ 'credits'
                     , 'ceqPeriod'
                     , 'satisfaction'
                     , 'relevance'
                     , 'quality'
                     , 'workload'
                     , 'registered'
                     , 'passed'
                     , 'percentPassed'
                     ]

    let order = this.previousSortKey === k? -1 : 1
    this.previousSortKey = k

    if (revProps.includes(k))
      order = -order

    let sortFunc

    if (k === 'ceqPeriod')
      sortFunc = sorter(x => x.ceqs[0].ceqPeriod, comparePeriod, order)
    else if (k === 'period')
      sortFunc = sorter(x => x.period.lp, compareDefault, order)
    else if (ceqProps.includes(k))
      sortFunc = sorter(x => x.ceqs[0][k], compareDefault, order)
    else
      sortFunc = sorter(x => x[k], compareDefault, order)

    this.setState({ courses: this.state.courses.slice().sort(sortFunc) })

    return order
  }

  render() {
    return (
      <table className='course-table'>
        <CourseHeader sortCallback={this.handleSort} />
        <tbody>
          {this.state.courses.map(c => {
            return <CourseRow key={c.code} data={c} />
          })}
        </tbody>
      </table>
    )
  }
}

function dataForCourse(rel) {
  let qs = db.ceqs[rel.code]

  if (!qs || !qs.length) {
    let as = db.aliases[rel.code] || []

    qs = []

    as.forEach(a => {
      qs = qs.concat(db.ceqs[a])
    })
  }

  qs.forEach(q => {
    if ((q.passed === 0 || q.passed) && q.registered)
      q.percentPassed = q.passed / q.registered
  })

  qs.sort((a, b) => -comparePeriod(a.ceqPeriod, b.ceqPeriod))

  if (qs.length === 0)
    qs = [{}]

  return Object.assign(
    {}, db.courses[rel.code], rel, { ceqs: qs })
}

function sorter(accessor, comparator, order) {
  return (a, b) => {
    let nullStat = checkNulls(a, b)

    if (nullStat !== null)
      return nullStat

    let av = accessor(a)
      , bv = accessor(b)

    nullStat = checkNulls(av, bv)

    if (nullStat !== null)
      return nullStat

    let ret = comparator(av, bv)

    return order * ret
  }
}

function checkNulls(a, b) {
  let aNull = (a === undefined || a === null || Number.isNaN(a))
    , bNull = (b === undefined || b === null || Number.isNaN(b))

  if (aNull && !bNull)
    return 1
  else if (!aNull && bNull)
    return -1
  else if (aNull && bNull)
    return 0
  else
    return null
}

function compareDefault(a, b) {
  if (a < b)
    return -1
  else if (a > b)
    return 1
  else
    return 0
}

function comparePeriod(a, b) {
  if (a[0] !== b[0])
    return a[0] > b[0]? 1 : -1
  else if (a[1] !== b[1])
    return a[1].toLowerCase() === 'ht' && b[1].toLowerCase() === 'vt'? 1 : -1
  else if (a[2] !== b[2])
    return a[2] > b[2]? 1 : -1
  else
    return 0
}

function compareArray(a, b) {
  for (let i = 0; ; i++) {
    let av = a[i]
      , bv = b[i]
      , nullStat = checkNulls(av, bv)

    if (nullStat !== null)
      return nullStat
    else if (av > bv)
      return 1
    else if (bv < av)
      return -1
  }
}

