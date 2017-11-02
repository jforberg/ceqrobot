import React from 'react'
import 'sprintf-js'
import Chart from 'chart.js'

export default class CourseRow extends React.Component {
  constructor(props) {
    super(props)

    this.state = { expanded: false }

    this.handleExpand = this.handleExpand.bind(this)
  }

  handleExpand() {
    this.setState({ expanded: !this.state.expanded })
  }

  render() {
    const courseFragmentSpan = 10
        , ceqFragmentSpan = 7

    let rowParity = this.props.index % 2 === 0? 'even' : 'odd'

      , firstRow = (
          <tr key={0} className={rowParity}>
            <CourseRowFragment data={this.props.data}
                               nameClicked={this.handleExpand} />
            <CeqRowFragment data={this.props.data.ceqs[0]} />
          </tr>
        )

      , ceqCount = this.props.data.ceqs.length

    if (this.state.expanded) {
      if (ceqCount < 2)
        return [ firstRow
               , <tr key={1} className={rowParity}>
                   <td colSpan={courseFragmentSpan + ceqFragmentSpan}
                       style={{verticalAlign: 'top'}}>
                     <CourseInfo data={this.props.data} />
                   </td>
                 </tr>
               ]
      else
        return [ firstRow
               , <tr key={1} className={rowParity}>
                   <td colSpan={courseFragmentSpan}
                       rowSpan={ceqCount - 1 + 1}
                       style={{verticalAlign: 'top'}}>
                     <CourseInfo data={this.props.data} />
                   </td>
                   <CeqRowFragment data={this.props.data.ceqs[1]} />
                 </tr>
               ].concat(
                 this.props.data.ceqs.slice(2).map((q, i) => (
                   <tr key={i + 2} className={rowParity}>
                     <CeqRowFragment data={q} />
                   </tr>
                 ))
               ).concat(
                 <tr key={99} className={`${rowParity} fillRow`}>
                   {[...Array(ceqFragmentSpan).keys()].map(i =>
                     <td key={i}></td>
                   )}
                 </tr>
               )
    } else {
      return firstRow
    }
  }
}

class CourseRowFragment extends React.Component {
  render() {
    function type(t) {
      switch (t) {
      case 'obligatory':
        return 'O'
      case 'altobligatory':
        return 'A'
      case 'elective':
        return 'V'
      default:
        return null
      }
    }

    function credits(c) {
      return ('' + c).replace('.', ',')
    }

    function level(lvl) {
      return lvl.replace('level', '')[0].toUpperCase()
    }

    function name(n) {
      const limit = 65
          , trailer = ' ...'

      if (n.length <= limit)
        return n

      let over = n.length - limit + trailer.length
        , ws = n.split(' ')

      function uselessWord(w) {
        return [ 'och', 'för' ].includes(w)
      }

      while (uselessWord(ws[ws.length - 1]) || over > 0) {
        let w = ws.pop()

        over -= w.length + 1
      }

      let str = ws.join(' ')

      if (str[str.length - 1] === ',')
        str = str.slice(0, str.length - 1)

      return str + trailer
    }

    function lp(p, i) {
      if (p.periodical)
        return (
          <abbr title='Periodiserad'>P</abbr>
        )
      else
        return p.lp[i]? '' + (i + 1) : ''
    }

    let c = this.props.data

    return (
      [ <td key={0}>
          {c.code}
        </td>
      , <td key={1}>
          {c.year}
        </td>
      , <td key={2}>
          {type(c.type)}
        </td>
      , <td key={3}
            style={{textAlign: 'right'}}>
          {credits(c.credits)}
        </td>
      , <td key={4}>
          {level(c.level)}
        </td>
      , <td key={5}
            onClick={this.props.nameClicked}
            className='courseName'>
          {name(c.name)}
        </td>
      , <td key={6}>
          {lp(c.period, 0)}
        </td>
      , <td key={7}>
          {lp(c.period, 1)}
        </td>
      , <td key={8}>
          {lp(c.period, 2)}
        </td>
      , <td key={9}>
          {lp(c.period, 3)}
        </td>
      ]
    )
  }
}

class CeqRowFragment extends React.Component {
  render() {
    function unknown() {
      return <span style={{color: '#aaa'}}>?</span>
    }

    function plusMin(n) {
      if (n === null || n === undefined)
        return unknown()
      else if (n < 0)
        return <span style={{color: 'rgb(' + (32 + 3*Math.abs(n)) + ', 0, 0)'}}>{n}</span>
      else
        return <span style={{color: 'rgb(0, ' + 2*Math.abs(n) + ', 0)'}}>+{n}</span>
    }

    function percent(n) {
      if (Number.isNaN(n))
        return unknown()

      let str = sprintf('%02d%%', n * 100)
        , goodness = Math.round(400 * Math.max(n - 0.5, 0))
        , badness = Math.round(700 * Math.abs(Math.min(n - 0.5, 0)))
        , colour = `rgb(${badness}, ${goodness}, 0)`

      return <span style={{color: colour}}>{str}</span>
    }

    function periodLink(ps, url) {
      if (!ps)
        return ''

      let str = formatCeqPeriod(ps)

      return (
        <a href={url}>{str}</a>
      )
    }

    function cTd(i, html) {
      return (
        <td key={i} style={{textAlign: 'center'}}>
          {html}
        </td>
      )
    }

    let q = this.props.data

    return (
      [ cTd('ceqPeriod', periodLink(q.ceqPeriod, q.url))
      , cTd('satisfaction', plusMin(q.satisfaction))
      , cTd('relevance', plusMin(q.relevance))
      , cTd('quality', plusMin(q.quality))
      , cTd('workload', plusMin(q.workload))
      , cTd('registered', q.registered)
      , cTd('percentPassed', percent(q.passed / q.registered))
      ]
    )
  }
}

class CourseInfo extends React.Component {
  render() {
    let aliasStr = ''
      , as = this.props.data.aliases

    if (as.length)
      aliasStr = 'aka. ' + as.join(', ')

    return (
      <div className='chartContainer'>
        <span>{aliasStr}</span>
        <div>
          <CourseChart data={this.props.data} />
        </div>
      </div>
    )
  }
}

class CourseChart extends React.Component {
  componentDidMount() {
    let ceqs = this.props.data.ceqs.slice().reverse()
      , labels = ceqs.map(q => formatCeqPeriod(q.ceqPeriod))
      , config =
          { type: 'line'
          , data:
              { labels: labels
              , datasets:
                  [ { label: 'Övl'
                    , borderColor: '#ff9999'
                    , data: ceqs.map(q => q.satisfaction)
                    , fill: false
                    }
                  , { label: 'Ang'
                    , borderColor: '#99ff99'
                    , data: ceqs.map(q => q.relevance)
                    , fill: false
                    , hidden: true
                    }
                  , { label: 'Udv'
                    , borderColor: '#9999ff'
                    , data: ceqs.map(q => q.quality)
                    , fill: false
                    , hidden: true
                    }
                  , { label: 'Arb'
                    , borderColor: '#ffff99'
                    , data: ceqs.map(q => q.workload)
                    , fill: false
                    , hidden: true
                    }
                  , { label: 'Reg'
                    , borderColor: '#99ffff'
                    , data: ceqs.map(q => q.registered)
                    , fill: false
                    , hidden: true
                    }
                  , { label: 'G%'
                    , borderColor: '#ff99ff'
                    , data: ceqs.map(q => Math.round(100 * q.percentPassed))
                    , fill: false
                    , hidden: true
                    }
                  ]
              }
          }

      , context = this.chartElement.getContext('2d')

    config.data.datasets.forEach(d => d.cubicInterpolationMode = 'monotone')

    this.chart = new Chart(context, config)
  }

  componentWillUmount() {
    this.chart.destroy()
    this.chart = null
  }

  render() {
    return <canvas ref={e => this.chartElement = e} />
  }
}

function formatCeqPeriod(ps) {
  let [y, s, p] = ps

  return sprintf('%s%s%s', y, s, p).toUpperCase()
}
