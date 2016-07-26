private
topicForClass: aClass

	^ ClassAPIHelpBuilder new
		addMethodsWithoutComment: false;
		rootToBuildFrom: aClass;
		build;
		topicToBuild