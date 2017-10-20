tests - repositories - places
testSmalltalkhub
	gofer smalltalkhubUser: 'dh83' project: 'ci'.
	self assert: gofer repositories: #('http://smalltalkhub.com/mc/dh83/ci/main/')