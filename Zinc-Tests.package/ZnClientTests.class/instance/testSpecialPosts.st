testing
testSpecialPosts
	self skip.
	self withServerDo: [ :server |
		{ ZnEntity with: 'A'. ZnEntity with: #[ 42 ]. ZnEntity with: ''. ZnEntity with: #[ ] } 
			do: [ :each |
				| client |
				(client := ZnClient new)
					url: server localUrl;
					addPath: #echo;
					entity: each;
					post.
				self assert: client isSuccess.
				self assert: (each isEmpty or: [client contents includesSubstring: each printString ]).
				client close.
				(client := ZnClient new)
					beOneShot;
					url: server localUrl;
					addPath: #echo;
					entity: each;
					post.
				self assert: client isSuccess.
				self assert: (each isEmpty or: [client contents includesSubstring: each printString ]) ] ]