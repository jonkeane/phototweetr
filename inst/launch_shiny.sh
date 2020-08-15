#! /bin/bash

phototweetrpath=$(pwd) phototweetrtweet=true R -e 'shiny::runApp(system.file("shiny", "tweetpreview", package = "phototweetr"), host = "127.0.0.1", port = 8181)'
