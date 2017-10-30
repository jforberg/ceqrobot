import React from 'react'

export default class CeqRowFragment extends React.Component {
  render() {
    let q = this.props.data

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

    function period(ps) {
      if (!ps)
        return ''

      let [y, s, p] = ps

      return sprintf('%s%s%s', y, s, p).toUpperCase()
    }

    function cTd(i, html) {
      return (
        <td key={i} style={{textAlign: 'center'}}>
          {html}
        </td>
      )
    }

    return (
      [ cTd(0, period(q.ceqPeriod))
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
