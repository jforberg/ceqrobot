import React from 'react'

import CourseTable from './CourseTable'

export default class Main extends React.Component {
  /*
  constructor (props) {
    super(props)
  }
   */

  render () {
    return (
      <div>
        <h1>CEQROBOT</h1>
        <CourseTable />
      </div>
    )
  }
}
