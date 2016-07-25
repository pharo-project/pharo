accessing
browsingHistory

	^ browsingHistory ifNil: [ browsingHistory := NavigationHistory new ]