import React from 'react'

import CourseTable from './CourseTable'

export default class Main extends React.Component {
  constructor (props) {
    super(props)

    this.state = { mode: 'courses'
                 }
  }

  render () {
    /*
          <ul className='menu'>
            <li className='menuSelected'>Kurser</li>
            <li>Optimera</li>
          </ul>
     */
    return (
      <div>
        <nav className='header'>
          <h1>CEQROBOT</h1>
          <span className='tagline'>Beslutsstöd för teknologer</span>
          <div className='clearFix'></div>
        </nav>
        <CourseTable />
      </div>
    )
  }
}
