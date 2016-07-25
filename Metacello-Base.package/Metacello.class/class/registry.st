instance creation
registry
    ^ self new
        executorSpec: #'MetacelloScriptRegistryExecutor' -> 'batch';
        yourself