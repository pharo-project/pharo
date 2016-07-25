instance creation
openWithEnvironment: anEnvironment removeTestCase: aBoolean
	self new
		items:
			(RBCompositeLintRule allGoodRules rules
				copyWithFirst: PharoCriticRules pharoIntegrationLintRule);
		environment: anEnvironment;
		removeTestCase: aBoolean;
		openWithSpec;
		updateTree