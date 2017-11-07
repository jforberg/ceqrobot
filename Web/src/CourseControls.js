import React from 'react'

const interestingProgrammes =
        [ 'A'
        , 'B'
        , 'BI'
        , 'BME'
        , 'C'
        , 'D'
        , 'E'
        , 'F'
        , 'I'
        , 'ID'
        , 'K'
        , 'L'
        , 'M'
        , 'MD'
        , 'N'
        , 'PI'
        , 'RH'
        , 'V'
        , 'W'
        ]
    , interestingMap = interestingProgrammes.reduce((acc, x) => {
        acc[x] = true
        return acc
      }, {})

export default class CourseControls extends React.Component {
  constructor(props) {
    super(props)

    this.programmes = Object.keys(db.programmes)

    this.state = { programme: 'F'
                 , masters: ''
                 }

    this.handleChange = this.handleChange.bind(this)
  }

  getCourseRels() {
    let masters = db.lotMap[this.state.programme]

    if (!masters)
      return []
    else
      return masters[this.state.masters] || []
  }

  handleChange(e) {
    let key = e.target.name
      , val = e.target.value
      , update = { [key]: val }

    if (key === 'programme' && val !== this.state.programme)
      update.masters = ''

    this.setState(update, () => {
      this.props.courseRelsCallback(this.getCourseRels())
    })
  }

  componentDidMount() {
    this.props.courseRelsCallback(this.getCourseRels())
  }

  render() {
    let programmesList = Object.keys(db.programmes)
          .filter(p => interestingMap[p])
          .sort()

      , mastersList = Object.keys(db.masters[this.state.programme])
          .concat([''])
          .filter(m => m !== 'EXJOBB')
          .sort()

      , progName = db.programmes[this.state.programme].name
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
