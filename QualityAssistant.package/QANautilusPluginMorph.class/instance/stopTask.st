task handling
stopTask

	updateTask ifNotNil: [ 
		updateTask 
  		terminate;
  		doInBackground: nil;
 		onCompleted: nil
	]