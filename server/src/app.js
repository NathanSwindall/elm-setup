const express = require('express')
const path = require('path')
const {join} = require('path')
const app = express()


// setup server on port 3001
const port = process.env.PORT || 3002


// set up the public directory to be served
const publicDirectory = path.join(__dirname,"../../client")
app.use(express.static(publicDirectory))

app.get('/data/json', (req, res) => res.json({username: "Nathan"}))

app.get('/data/json2', (req, res) => res.json(
    [
        {
          "url": "1.jpeg",
          "size": 36,
          "title": "Beachside"
        },
        {
          "url": "2.jpeg",
          "size": 19,
          "title": "Epica, live at the Agora"
        },
        {
          "url": "3.jpeg",
          "size": 41
        },
        {
          "url": "4.jpeg",
          "size": 41,
          "title": "City Museum"
        },
        {
          "url": "5.jpeg",
          "size": 25,
          "title": null
        },
        {
          "url": "6.jpeg",
          "size": 37,
          "title": "Boat in Glass"
        }
      ]
))

app.get('/data/json3', (req,res) => res.send("1.jpeg,2.jpeg,3.jpeg,4.jpeg,5.jpeg,6.jpeg"))

// start up server
app.listen(port,()=> {
    console.log("This app is running on port " + port)
})


