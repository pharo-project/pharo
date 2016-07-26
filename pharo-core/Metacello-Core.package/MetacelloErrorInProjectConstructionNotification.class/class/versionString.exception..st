instance creation
versionString: aString exception: anException
    ^ self new
        versionString: aString;
        exception: anException;
        signal