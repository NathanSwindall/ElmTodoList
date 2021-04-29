const express = require('express')
const path = require('path')
const {join} = require('path')
const app = express()


// setup server on port 3001
const port = process.env.PORT || 3002


// set up the public directory to be served
const publicDirectory = path.join(__dirname,"../../client")
app.use(express.static(publicDirectory))



// start up server
app.listen(port,()=> {
    console.log("This app is running on port " + port)
})