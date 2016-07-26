handlers
handleWarning: exception
  ^ (self options at: #'onWarning' ifAbsent: [ ^ exception pass ])
    cull: exception