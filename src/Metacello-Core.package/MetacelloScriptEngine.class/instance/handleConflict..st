handlers
handleConflict: exception
    ^ (self options at: #'onConflict' ifAbsent: [ ^ exception pass ])
        cull: exception
        cull: exception existingProjectRegistration
        cull: exception newProjectRegistration