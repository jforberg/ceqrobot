import React from 'react'
import 'sprintf-js'

export default class CourseRow extends React.Component {
  constructor(props) {
    super(props)

    this.state = db.courses[props.code]
    this.state.code = props.code
  }

  render() {
    let ceq = db.ceqs[this.state.code][0] || {}

    function credits(c) {
      return ('' + c).replace('.', ',')
    }

    function level(lvl) {
      return lvl.replace('level', '')[0].toUpperCase()
    }

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
      if (isNaN(n))
        return unknown()
      else
        return sprintf('%02d%%', n * 100)
    }

    function cTd(html) {
      return (
        <td style={{textAlign: 'center'}}>
          {html}
        </td>
      )
    }

    return (
      <tr>
        <td style={{fontFamily: 'bitstream vera sans mono'}}>
          {this.state.code}
        </td>
        <td style={{textAlign: 'right'}}>
          {credits(this.state.credits)}
        </td>
        <td>
          {level(this.state.level)}
        </td>
        <td>
          {this.state.name}
        </td>
        {cTd(plusMin(ceq.satisfaction))}
        {cTd(plusMin(ceq.relevance))}
        {cTd(plusMin(ceq.quality))}
        {cTd(plusMin(ceq.workload))}
        {cTd(ceq.registered)}
        {cTd(percent(ceq.passed / ceq.registered))}
      </tr>
    )
  }
}
