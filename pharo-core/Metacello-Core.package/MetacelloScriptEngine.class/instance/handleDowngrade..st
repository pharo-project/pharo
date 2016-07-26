handlers
handleDowngrade: exception
    ^ (self options at: #'onDowngrade' ifAbsent: [ ^ exception pass ])
        cull: exception
        cull: exception existingProjectRegistration
        cull: exception newProjectRegistration