private
ensureMetacello: loadList
  "Bootstrap Metacello, retry using alternate repository, if primary repository is not accessible"

  Smalltalk
    at: #'MetacelloProject'
    ifAbsent: [ 
      | version error gofer |
      (Array
        with: 'http://smalltalkhub.com/mc/dkh/metacello/main'
        with: 'http://seaside.gemtalksystems.com/ss/metacello')
        do: [ :repositoryUrl | 
          "bootstrap Metacello"
          [ 
          self bootstrapMetacelloFrom: repositoryUrl.
          Smalltalk
            at: #'ConfigurationOfMetacello'
            ifAbsent: [ 
              self
                retry: [ 
                  gofer := (Smalltalk at: #'Gofer') new.
                  gofer
                    perform: #'url:' with: repositoryUrl;
                    perform: #'package:' with: 'ConfigurationOfMetacello';
                    perform: #'load' ] ].
          version := (Smalltalk at: #'ConfigurationOfMetacello') project
            version: #'previewBootstrap'.
          version load: loadList.
          self
            retry: [ 
              gofer := (Smalltalk at: #'Gofer') new.
              Smalltalk
                at: #'ConfigurationOfMetacelloPreview'
                ifAbsent: [ 
                  gofer
                    perform: #'url:' with: repositoryUrl;
                    perform: #'package:'
                      with: 'ConfigurationOfMetacelloPreview';
                    perform: #'load' ] ].
          version := (Smalltalk at: #'ConfigurationOfMetacelloPreview') project
            version: #'stable'.	"load latest from GitHub"
          version load: loadList.
          ^ self ]
            on: Error
            do: [ :ex | 
              error := ex.
              Transcript
                cr;
                show: 'failed ensureMetacello using ';
                show: repositoryUrl printString;
                show: ' : ';
                show: ex description printString;
                show: '...retrying'.	"try again"
              ex return: nil ] ].	"shouldn't get here unless the load failed ... throw an error"
      self
        error:
          'retry with alternate repository failed: ' , error description printString ]