tests
testCopyWithCorrectBasename
        | directory |
        self
                markForCleanup: 'gooly';
                markForCleanup: 'plonk'.
        directory := filesystem workingDirectory.
        (directory / 'gooly') ensureCreateFile.
        directory / 'gooly' copyTo: directory / 'plonk'.
        self assert: (directory / 'plonk') exists.
        self assert: (directory childNames includes: 'plonk')