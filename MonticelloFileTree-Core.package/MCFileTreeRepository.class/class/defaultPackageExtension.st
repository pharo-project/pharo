accessing
defaultPackageExtension
    ".tree, .pkg, .package are the only formats supported at the moment:
	.tree         - original structure
	.pkg          - snapshot structure
	.package - cypress structure"

    defaultPackageExtension
        ifNil: [ 
            defaultPackageExtension := MCFileTreePackageStructureStWriter useCypressWriter
                ifTrue: [ '.package' ]
                ifFalse: [ '.pkg' ] ].
    ^ defaultPackageExtension