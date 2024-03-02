# Dashboard4HouseBuyers

Collaborate with classmates from GEOM90007, which contains the source code part of the GEOM90007 course at the University of Melbourne.

## Run the app using:
`shinyApp(ui = ui, server = server)`

## Project Design
### UI Design
![64e628c3499760ce8ea795bd221e0fe](https://github.com/KerrYuan/Dashboard4HouseBuyers/assets/112842070/71a52c5c-a0bc-4827-b70b-6695e5a2c3b0)

## Project Structure
```
Dashboard4HouseBuyers/
|-- www/                 # Directory for static resources
|   |-- images/          # Images used in the app
|   |   |-- logo.png
|   |-- styles/          # Custom CSS styles
|   |   |-- style.css
|-- data/                # Data used in the app
|   |-- dataset1.csv
|-- modules/             # Shiny modules (if needed)
|   |-- module1.R
|-- global.R             # Loads required libraries and data, and initializes variables
|-- ui.R                 # Defines the UI of the app
|-- server.R             # Contains the server logic
```

