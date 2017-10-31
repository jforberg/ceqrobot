import React from 'react'
import 'sprintf-js'

//import CeqRowFragment from './CeqRowFragment'

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
    let firstRow = (
          <tr key={0}>
            <CourseRowFragment data={this.props.data}
                               nameClicked={this.handleExpand} />
            <CeqRowFragment data={this.props.data.ceqs[0]} />
          </tr>
        )
      , ceqCount = this.props.data.ceqs.length

    if (this.state.expanded) {
      if (ceqCount < 2)
        return [ firstRow
               , <tr key={1}>
                   <td colSpan={8} style={{verticalAlign: 'top'}}>
                     Expanded
                   </td>
                 </tr>
               ]
      else
        return [ firstRow
               , <tr key={1}>
                   <td colSpan={8} rowSpan={ceqCount - 1}
                       style={{verticalAlign: 'top'}}>
                     Expanded
                   </td>
                   <CeqRowFragment data={this.props.data.ceqs[1]} />
                 </tr>
               ].concat(
                 this.props.data.ceqs.slice(2).map((q, i) => (
                   <tr key={i + 2}>
                     <CeqRowFragment data={q} />
                   </tr>
                 ))
               )
    } else {
      return firstRow
    }
  }
}

class CourseRowFragment extends React.Component {
  render() {
    function credits(c) {
      return ('' + c).replace('.', ',')
    }

    function level(lvl) {
      return lvl.replace('level', '')[0].toUpperCase()
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
      , <td key={1} style={{textAlign: 'right'}}>
          {credits(c.credits)}
        </td>
      , <td key={2}>
          {level(c.level)}
        </td>
      , <td key={3} onClick={this.props.nameClicked}>
          {c.name}
        </td>
      , <td key={4}>
          {lp(c.period, 0)}
        </td>
      , <td key={5}>
          {lp(c.period, 1)}
        </td>
      , <td key={6}>
          {lp(c.period, 2)}
        </td>
      , <td key={7}>
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
      else
        return sprintf('%02d%%', n * 100)
    }

    function periodLink(ps, url) {
      if (!ps)
        return ''

      let [y, s, p] = ps
        , str = sprintf('%s%s%s', y, s, p).toUpperCase()

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
      [ cTd(0, periodLink(q.ceqPeriod, q.url))
      , cTd(1, plusMin(q.satisfaction))
      , cTd(2, plusMin(q.relevance))
      , cTd(3, plusMin(q.quality))
      , cTd(4, plusMin(q.workload))
      , cTd(5, q.registered)
      , cTd(6, percent(q.passed / q.registered))
      ]
    )
  }
}

class CourseInfo extends React.Component {
}
