icon
classIconFor: aClass
	(aClass isTestCase and: [ aClass isAbstract not and: [ aClass hasTestSelectors ] ])
		ifTrue: [ | icon button |
			icon :=  self iconNamed: #testNotRunIcon.
			aClass hasPassedTest ifTrue: [ icon := self iconNamed: #testGreenIcon ].
			aClass hasFailedTest ifTrue: [ icon := self iconNamed: #testYellowIcon ].
			aClass hasErrorTest ifTrue: [ icon := self iconNamed: #testRedIcon ].
					
			button := IconicButton new 
						target: self;
						actionSelector: #runTestsOfClass:notifying:;
						arguments: { aClass. true };
						labelGraphic: icon ;
						color: Color transparent;
						extent: 12 @ 12;
						helpText: 'Run the tests';
						borderWidth: 0.
			^ button ]
		ifFalse: [
			(aClass hasComment not )
				ifTrue: [| button |
					button := IconicButton new 
								target: self;
								actionSelector: #openCommentEditor:;
								arguments: { aClass };
								labelGraphic: (self iconNamed: #uncommentedClassIcon) ;
								color: Color transparent;
								extent: 12 @ 12;
								helpText:'Edit the comment';
								borderWidth: 0.
					^ button ]].

	^ aClass systemIcon