handlers
handleEnsureProjectLoadedForDevelopment: exception
    "if useCurrentVersion resume with true, else resume with false"

    ^ exception resume: self useCurrentVersion