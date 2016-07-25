compiling
compile: sourceString

       text := sourceString.
       self class
               compileSilently: text
               classified: 'generated'
               notifying: self