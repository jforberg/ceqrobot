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

  render() {
    let sortOrder = (k) => this.sortKey === k? this.sortDir : null

    return (
      <thead>
        <tr>
          <SortHead cb={this.handleSort}
                    k='code'
                    n='KK'
                    s={sortOrder('code')} />
          <SortHead cb={this.handleSort}
                    k='year'
                    n='Å'
                    t='Ingår i årskurs'
                    colSpan={1}
                    s={sortOrder('year')} />
          <SortHead cb={this.handleSort}
                    k='type'
                    n='T'
                    t='O = obligatorisk, A = alternativobligatorisk, V = valfri'
                    colSpan={1}
                    s={sortOrder('type')} />
          <SortHead cb={this.handleSort}
                    k='credits'
                    n='HP'
                    colSpan={1}
                    s={sortOrder('credits')} />
          <SortHead cb={this.handleSort}
                    k='level'
                    n='N'
                    t='G = grundnivå, A = avancerad nivå'
                    s={sortOrder('level')} />
          <SortHead cb={this.handleSort}
                    k='name'
                    n='Namn'
                    s={sortOrder('name')} />
          <SortHead cb={this.handleSort}
                    k='period'
                    n='LP'
                    s={sortOrder('period')}
                    colSpan={4} />
          <SortHead cb={this.handleSort}
                    k='ceqPeriod'
                    n='CEQ'
                    s={sortOrder('ceqPeriod')}
                    t='Senaste CEQ-period' />
          <SortHead cb={this.handleSort}
                    k='satisfaction'
                    n='Övl'
                    s={sortOrder('satisfaction')}
                    t='"Överlag är jag nöjd med den här kursen"' />
          <SortHead cb={this.handleSort}
                    k='relevance'
                    n='Ang'
                    s={sortOrder('relevance')}
                    t='"Kursen känns angelägen för min utbildning"' />
          <SortHead cb={this.handleSort}
                    k='quality'
                    n='Udv'
                    s={sortOrder('quality')}
                    t='"God undervisning"' />
          <SortHead cb={this.handleSort}
                    k='workload'
                    n='Arb'
                    s={sortOrder('workload')}
                    t='"Lämplig arbetsbelastning"' />
          <SortHead cb={this.handleSort}
                    k='registered'
                    n='Reg'
                    s={sortOrder('registered')}
                    t='Antal kursregistrerade studenter' />
          <SortHead cb={this.handleSort}
                    k='percentPassed'
                    n='G%'
                    s={sortOrder('percentPassed')}
                    t='Andel godkända studenter' />
        </tr>
      </thead>
    )
  }
}

class SortHead extends React.Component {
  render() {
    let sortByKey = e => this.props.cb(this.props.k, e)

    return (
      <th colSpan={this.props.colSpan}
          onClick={sortByKey}>
        <a className='sortControl'>
          <abbr title={this.props.t}>{this.props.n}</abbr>
          <span className='sortindicator'>
            {this.props.s? this.props.s > 0? '▲' : '▼' : ''}
          </span>
        </a>
      </th>
    )
  }
}


