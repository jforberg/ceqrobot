import React from 'react'

export default class CourseHeader extends React.Component {
  render() {
    return (
      <thead>
        <tr>
          <th colSpan={4}>Kurs</th>
          <th>
            <abbr title='"Överlag är jag nöjd med den här kursen"'>Övl</abbr>
          </th>
          <th>
            <abbr title='"Kursen känns angelägen för min utbildning"'>Ang</abbr>
          </th>
          <th>
            <abbr title='"God undervisning"'>Udv</abbr>
          </th>
          <th>
            <abbr title='"Lämplig arbetsbelastning"'>Arb</abbr>
          </th>
          <th>
            <abbr title='Antal kursregistrerade studenter'>Reg</abbr>
          </th>
          <th>
            <abbr title='Andel godkända studenter'>G%</abbr>
          </th>
        </tr>
      </thead>
    )
  }
}
