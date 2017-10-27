import React from 'react'

import CourseRow from './CourseRow'
import CourseHeader from './CourseHeader'

export default class CourseTable extends React.Component {
  constructor(props) {
    super(props)

    this.state = { courses: db.courses
                 }

    this.handleSort = this.handleSort.bind(this)
  }

  handleSort(key) {
  }

  render() {
    return (
      <table>
        <CourseHeader />
        <tbody>
          {Object.keys(db.courses).map((c, i) => {
            return <CourseRow key={i} code={c} />
          })}
        </tbody>
      </table>
    )
  }
}
