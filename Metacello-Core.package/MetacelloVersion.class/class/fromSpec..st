instance creation
fromSpec: aMetacelloVersionSpec
    ^ (self new versionNumber: aMetacelloVersionSpec versionNumber)
        spec: aMetacelloVersionSpec;
        yourself