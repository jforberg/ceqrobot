import React from 'react'

export default class TableControls extends React.Component {
  constructor(props) {
    super(props)

    this.programmes = Object.keys(db.programmes)

    this.state = { programme: 'F'
                 , masters: ''
                 }

    this.handleChange = this.handleChange.bind(this)
  }

  getCourseRels() {
    return db.programmes[this.state.programme][this.state.masters]
  }

  handleChange(e) {
    let key = e.target.name
      , val = e.target.value

    this.setState({ [key]: val }, () => {
      this.props.courseRelsCallback(this.getCourseRels())
    })
  }

  componentDidMount() {
    this.props.courseRelsCallback(this.getCourseRels())
  }

  render() {
    let programmesList = Object.keys(db.programmes)
      , mastersList = Object.keys(db.programmes[this.state.programme])

      , progName = 'Teknisk fysik'
      , mastersName = this.state.masters === ''? 'Hela programmet' :
          db.masters[this.state.programme][this.state.masters].name

      , mCode = n => n === ''? '*' : n

      , progRadios = programmesList.map(p => (
          <label key={p}
                 className={this.state.programme === p? 'selected' : ''}>
            <input type='radio'
                   name='programme'
                   value={p}
                   checked={this.state.programme === p}
                   onChange={this.handleChange} />
            {p}
          </label>))

      , mastersRadios = mastersList.map(m => (
          <label key={m}
                 className={this.state.masters === m? 'selected' : ''}>
            <input type='radio'
                   name='masters'
                   value={m}
                   checked={this.state.masters === m}
                   onChange={this.handleChange} />
            {mCode(m)}
          </label>))

    return (
      <form className='tableControls'>
        <div className='tableControlRow'>
          <b>PROG:</b>
          {progRadios}
          <small className='tableControlDisplay'>
            {progName}
          </small>
        </div>
        <div className='tableControlRow'>
          <b>SPEC:</b>
          {mastersRadios}
          <small className='tableControlDisplay'>
            {mastersName}
          </small>
        </div>
      </form>
    )
  }
}
