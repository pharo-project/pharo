private
scriptExecutorClass: anExecutorSpec
    Smalltalk at: anExecutorSpec key ifAbsent: [ ConfigurationOf ensureMetacello: anExecutorSpec value ].
    ^ Smalltalk at: anExecutorSpec key