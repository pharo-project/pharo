specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		add: #(model workingCopies) right: 0.5;
		add: #(model repositories) left: 0.5;
		yourself.
