import React from 'react'
import 'sprintf-js'

import CeqRowFragment from './CeqRowFragment'

export default class CourseRow extends React.Component {
  constructor(props) {
    super(props)

    this.expanded = false
  }

  render() {
    let c = this.props.data

    function credits(c) {
      return ('' + c).replace('.', ',')
    }

    function level(lvl) {
      return lvl.replace('level', '')[0].toUpperCase()
    }

    return (
      <tr>
        <td style={{fontFamily: 'bitstream vera sans mono'}}>
          {c.code}
        </td>
        <td style={{textAlign: 'right'}}>
          {credits(c.credits)}
        </td>
        <td>
          {level(c.level)}
        </td>
        <td>
          {c.name}
        </td>
        <CeqRowFragment data={this.props.data.ceq} />
      </tr>
    )
  }
}
