private
checkAllowed
  self existingProjectRegistration locked
    ifTrue: [ 
      self
        resume:
          (MetacelloAllowLockedProjectChange new
            operationString: self operationString;
            existingProjectRegistration: self existingProjectRegistration;
            newProjectRegistration: self newProjectRegistration;
            signal) ]