tests
doubleResumeTest

       [self doSomething.
       MyResumableTestError signal.
       self doSomethingElse.
       MyResumableTestError signal.
       self doYetAnotherThing]
               on: MyResumableTestError
               do: [:ex | ex resume].