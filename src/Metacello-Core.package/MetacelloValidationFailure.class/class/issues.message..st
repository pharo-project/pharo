instance creation
issues: anObject message: aString
    ^ self new
        issues: anObject;
        signal: aString