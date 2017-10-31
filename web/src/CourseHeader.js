import React from 'react'

export default class CourseHeader extends React.Component {
  constructor(props) {
    super(props)

    this.handleSort = this.handleSort.bind(this)
  }

  handleSort(k, e) {
    let dir = this.props.sortCallback(k, e)
    this.sortKey = k
    this.sortDir = dir
  }

  sortMarker(key) {
    if (this.sortKey !== key)
      return ''

    let icon = this.sortDir === 1? '▲' : '▼'

    return icon
  }

  render() {
    let sortByKey = k => e => this.handleSort(k, e)

    return (
      <thead>
        <tr>
          <th style={{textAlign: 'left'}}
              onClick={sortByKey('code')}>
            KK
            {this.sortMarker('code')}
          </th>
          <th colSpan={2}
              onClick={sortByKey('credits')}>
            HP
            {this.sortMarker('credits')}
          </th>
          <th onClick={sortByKey('name')}>
            Namn
            {this.sortMarker('name')}
          </th>
          <th colSpan={4} onClick={sortByKey('period')}>
            LP
            {this.sortMarker('period')}
          </th>
          <th onClick={sortByKey('ceqPeriod')}>
            <abbr title='Senaste CEQ-period'>CEQ</abbr>
            {this.sortMarker('ceqPeriod')}
          </th>
          <th onClick={sortByKey('satisfaction')}>
            <abbr title='"Överlag är jag nöjd med den här kursen"'>Övl</abbr>
            {this.sortMarker('satisfaction')}
          </th>
          <th onClick={sortByKey('relevance')}>
            <abbr title='"Kursen känns angelägen för min utbildning"'>Ang</abbr>
            {this.sortMarker('relevance')}
          </th>
          <th onClick={sortByKey('quality')}>
            <abbr title='"God undervisning"'>Udv</abbr>
            {this.sortMarker('quality')}
          </th>
          <th onClick={sortByKey('workload')}>
            <abbr title='"Lämplig arbetsbelastning"'>Arb</abbr>
            {this.sortMarker('workload')}
          </th>
          <th onClick={sortByKey('registered')}>
            <abbr title='Antal kursregistrerade studenter'>Reg</abbr>
            {this.sortMarker('registered')}
          </th>
          <th onClick={sortByKey('percentPassed')}>
            <abbr title='Andel godkända studenter'>G%</abbr>
            {this.sortMarker('percentPassed')}
          </th>
        </tr>
      </thead>
    )
  }
}
