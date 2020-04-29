# documan

Utility to generate documents and diagrams.

Currently, only generation of sequence diagrams is supported. 

Sequence diagrams can be created using a Domain Specific Language (DSL), which looks like pseudo-code (please see below for syntax):

### Input

![Source Code in DSL](images/source.png)

### Output

![Generated Sequence Diagram](images/generated.png)

Colors can be customized using a json configuration file:
![Generated Sequence Diagram](images/custom-colors.png)

```json
{
  "theme": {
    "bg": "#e8f9df",
    "actor": {
      "fill": "#b7d03b",
      "text": "#000000",
      "bar": "#596c29"
    },
    "gui": {
      "fill": "#bcffdb",
      "text": "#000000",
      "bar": "#4f7942"
    },
    "server": {
      "fill": "#3fc283",
      "text": "#000000",
      "bar": "#0f4f34"
    },
    "external": {
      "fill": "#3b5c0a",
      "text": "#d2d83f",
      "bar": "#3b5c0a"
    },
    "flow-title": {
      "text": "#3b5c0a"
    },
    "flow-placeholder": {
      "fill": "#d2d83f",
      "text": "#000000",
      "line-color": "#046307"
    }
  }
}
```

#### Disclaimer
* I am a Clojure newbie, so the implementation is far from ideal; I will keep updating/improving as I learn more of Clojure. 

* Currently, there are no tests.

---

### Installation

Clojure 1.8+, Java 12+ and Leiningen 2.8.1 are required.



### Usage

The source files (**.documan**) need to be created in src folder. For details, please check the sample projects. 

Sublime Text 3 syntax-highlighting file is included.

The colors can be customized for each project using a config.json file. Please check the sample projects for details.

To generate all diagrams in a project:

    $ lein run <project-path> [-t] [-g]

* Option **-t** will transpile the .documan files in src folder into .edn files in edn folder.

* Option **-g** will generate the documents in dst folder from the .edn files in edn folder.

* If none of these options are specified, both transpiling and generation are done.

**Examples:**

    $ lein run projects/sample-1
    $ lein run projects/sample-2 -t

---
### DSL Syntax

First line needs to be the type of the document and title.
    
    Sequence-Diagram V<version> "<title>"

where, title can contain spaces and quotes, preceding and trailing spaces will be trimmed.

Strings can be split to multi-lines by embedding \n inside them.

    "Line 1 \n Line 2"

For each line preceding and trailing spaces will be trimmed.

Objects need to be defined in the same order they are to be rendered (from left to right).
They need to be declared at the beginning, before the flows.

    <id> = <tag> "<name>"
   
where, id needs to be unique. name can contain spaces and quotes, preceding and trailing spaces will be trimmed. tag can be one of the following: **actor**, **gui**, **server** or **external**

Syntax of a flow:

    <id> = <object-id> "<name>" (
        <flow-items>
    )

**Syntax of flow items:**

Call another module, with return value specified:

    => <object-id> "<name>" {
        <flow-items>
        return "<return-value>"	
    }

Call another module, without returning anything:
    
    => <object-id> | self "<name>"

Processing or calling a function in the same object:
    
    => self "<name>"

Send a message to another object:

    -> <object-id> "<name>"

Async call to another flow:

    >> <flow-id> "<name>"

---

### Libraries Used

* **dali** https://github.com/stathissideris/dali is used for generating SVG files.
* **data.json** https://github.com/clojure/data.json is used to read config.json file.


### License

Copyright © 2020 Manoj Kumar A

Distributed under the Eclipse Public License version 1.0
