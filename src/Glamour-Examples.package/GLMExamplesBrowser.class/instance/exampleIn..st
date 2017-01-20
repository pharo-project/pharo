building
exampleIn: composer
	| dynamicPresentation |
	dynamicPresentation := composer dynamic
		title: 'Browser';
		display:
				[ :exampleClass :examplePragma | self exampleBrowserForPragma: examplePragma in: exampleClass ].
	self
		flag:
			'openTree is a method that depends on Roassal. It is not loaded in the core version of Glamour. A better solution is needed here for loading this actions maybe based on pragmas.'.
	(GLMBrowser canUnderstand: #openTree)
		ifTrue: [ dynamicPresentation
				act: [ :dynamic | 
					dynamic cachedPresentation presentations first panes first
						presentations first openTree ]
				entitled: 'View browser tree' ].
	composer pharoMethod
		title: 'Source code';
		act: [ :text :exampleClass :examplePragma | 
			Smalltalk tools browser
				openOnClass: exampleClass
				selector: examplePragma methodSelector ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			entitled: 'Browse';
		smalltalkClass: [ :exampleClass | exampleClass ];
		doItReceiver: [ :exampleClass | exampleClass ];
		display:
				[ :exampleClass :pragma | exampleClass sourceCodeAt: pragma methodSelector ]