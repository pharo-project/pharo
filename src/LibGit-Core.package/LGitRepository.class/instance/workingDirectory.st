accessing
workingDirectory
	^ workingDirectory ifNil: [
			workingDirectory := self withReturnHandlerDo: [
				(self repository_workdir: self) asFileReference ] ]