private
rebuildFieldAccessors
    "compatibility method"
    Author useAuthor: 'UFFIGenerator' during: [ 
        self 
            compileFields: self fields 
            withAccessors: true]