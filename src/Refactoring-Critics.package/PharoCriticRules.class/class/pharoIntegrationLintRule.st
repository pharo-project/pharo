accessing
pharoIntegrationLintRule
	"A set of rules used by the CI monkey to check code before its integration into Pharo"
	^ (RBCompositeLintRule rules: (self pharoHardLintRules collect: #new))
		name: 'Pharo integration rules';
		yourself