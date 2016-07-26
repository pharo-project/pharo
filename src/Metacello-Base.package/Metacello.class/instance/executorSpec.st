accessing
executorSpec
    executorSpec ifNil: [ executorSpec := #'MetacelloScriptApiExecutor' -> 'batch' ].
    ^ executorSpec