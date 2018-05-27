"
A minimal FTP client program.  Could store all state in inst vars, and use an instance to represent the full state of a connection in progress.  But simpler to do all that in one method and have it be a complete transaction.

Always operates in passive mode (PASV).  All connections are initiated from client in order to get through firewalls.

See ServerDirectory openFTP, ServerDirectory getFileNamed:, ServerDirectory putFile:named: for examples of use.

See TCP/IP, second edition, by Dr. Sidnie Feit, McGraw-Hill, 1997, Chapter 14, p311.
"
Class {
	#name : #FTPClient,
	#superclass : #TelnetProtocolClient,
	#instVars : [
		'dataSocket'
	],
	#category : #'Network-Protocols-Protocols'
}

{ #category : #accessing }
FTPClient class >> defaultPortNumber [
	^21
]

{ #category : #accessing }
FTPClient class >> logFlag [
	^#ftp
]

{ #category : #accessing }
FTPClient class >> rawResponseCodes [
	#(200 'Command okay.'
	500 'Syntax error, command unrecognized. This may include errors such as command line too long.'
	501 'Syntax error in parameters or arguments.'
	202 'Command not implemented, superfluous at this site.'
	502 'Command not implemented.'
	503 'Bad sequence of commands.'
	504 'Command not implemented for that parameter.'
	110 'Restart marker reply. In this case, the text is exact and not left to the particular implementation; it must read: MARK yyyy = mmmm Where yyyy is User-process data stream marker, and mmmm server''s equivalent marker (note the spaces between markers and "=").'
	211 'System status, or system help reply.'
	212 'Directory status.'
	213 'File status.'
	214 'Help message. On how to use the server or the meaning of a particular non-standard command. This reply is useful only to the human user.'
	215 'NAME system type. Where NAME is an official system name from the list in the Assigned Numbers document.'
	120 'Service ready in nnn minutes.'

	220 'Service ready for new user.'
	221 'Service closing control connection. Logged out if appropriate.'
	421 'Service not available, closing control connection. This may be a reply to any command if the service knows it must shut down.'
	125 'Data connection already open; transfer starting.'
	225 'Data connection open; no transfer in progress.'
	425 'Can''t open data connection.'
	226 'Closing data connection. Requested file action successful (for example, file transfer or file abort).'
	426 'Connection closed; transfer aborted.'
	227 'Entering Passive Mode (h1,h2,h3,h4,p1,p2).'

	230 'User logged in, proceed.'
	530 'Not logged in.'
	331 'User name okay, need password.'
	332 'Need account for login.'
	532 'Need account for storing files.'
	150 'File status okay; about to open data connection.'
	250 'Requested file action okay, completed.'
	257 '"PATHNAME" created.'
	350 'Requested file action pending further information.'
	450 'Requested file action not taken. File unavailable (e.g., file busy).'
	550 'Requested action not taken. File unavailable (e.g., file not found, no access).'
	451 'Requested action aborted. Local error in processing.'
	551 'Requested action aborted. Page type unknown.'
	452 'Requested action not taken. Insufficient storage space in system.'
	552 'Requested file action aborted. Exceeded storage allocation (for current directory or dataset).'
	553 'Requested action not taken. File name not allowed.')

]

{ #category : #protocol }
FTPClient >> abortDataConnection [
	self sendCommand: 'ABOR'.
	self closeDataSocket
]

{ #category : #protocol }
FTPClient >> ascii [
	self sendCommand: 'TYPE A'.
	self lookForCode: 200
]

{ #category : #protocol }
FTPClient >> binary [
	self sendCommand: 'TYPE I'.
	self lookForCode: 200
]

{ #category : #protocol }
FTPClient >> changeDirectoryTo: newDirName [
	self sendCommand: 'CWD ' , newDirName.
	self checkResponse.

]

{ #category : #private }
FTPClient >> closeDataSocket [
	self dataSocket
		ifNotNil: [
			self dataSocket closeAndDestroy.
			self dataSocket: nil]

]

{ #category : #private }
FTPClient >> dataSocket [
	^dataSocket
]

{ #category : #private }
FTPClient >> dataSocket: aSocket [
	dataSocket := aSocket
]

{ #category : #protocol }
FTPClient >> deleteDirectory: dirName [
	self sendCommand: 'RMD ' , dirName.
	self checkResponse.

]

{ #category : #protocol }
FTPClient >> deleteFileNamed: fileName [
	self sendCommand: 'DELE ' , fileName.
	self checkResponse.

]

{ #category : #'private protocol' }
FTPClient >> get: limit dataInto: dataStream [
	"Reel in data until the server closes the connection or the limit is reached.
	At the same time, watch for errors on otherSocket."

	| buf bytesRead currentlyRead |
	currentlyRead := 0.
	buf := String new: 4000.
	[currentlyRead < limit and: 
	[self dataSocket isConnected or: [self dataSocket dataAvailable]]]
		whileTrue: [
			self checkForPendingError.
			bytesRead := self dataSocket receiveDataWithTimeoutInto: buf.
			1 to: (bytesRead min: (limit - currentlyRead)) do: [:ii | dataStream nextPut: (buf at: ii)].
			currentlyRead := currentlyRead + bytesRead].
	dataStream reset.	"position: 0."
	^ dataStream
]

{ #category : #'private protocol' }
FTPClient >> getData [

	| dataStream |
	dataStream := RWBinaryOrTextStream on: (String new: 4000).
	self getDataInto: dataStream.
	self closeDataSocket.
	^dataStream contents

]

{ #category : #'private protocol' }
FTPClient >> getDataInto: dataStream [
	"Reel in all data until the server closes the connection.  At the same time, watch for errors on otherSocket.  Don't know how much is coming.  Put the data on the stream."

	| buf bytesRead |
	buf := String new: 4000.
	[self dataSocket isConnected or: [self dataSocket dataAvailable]]
		whileTrue: [
			self checkForPendingError.
			bytesRead := self dataSocket receiveDataWithTimeoutInto: buf.
			1 to: bytesRead do: [:ii | dataStream nextPut: (buf at: ii)]].
	dataStream reset.	"position: 0."
	^ dataStream
]

{ #category : #protocol }
FTPClient >> getDirectory [
	| dirList |
	self openPassiveDataConnection.
	self sendCommand: 'LIST'.
	dirList := self getData.
	self checkResponse.
	self checkResponse.
	^dirList

]

{ #category : #protocol }
FTPClient >> getFileList [
	| dirList |
	self openPassiveDataConnection.
	self sendCommand: 'NLST'.
	dirList := self getData.
	self checkResponse.
	self checkResponse.
	^dirList

]

{ #category : #protocol }
FTPClient >> getFileNamed: remoteFileName [
	| data |
	self openPassiveDataConnection.
	self sendCommand: 'RETR ', remoteFileName.
	[self checkResponse]
		on: TelnetProtocolError
		do: [:ex |
			self closeDataSocket.
			ex pass].
	data := self getData.
	self checkResponse.
	^data

]

{ #category : #protocol }
FTPClient >> getFileNamed: remoteFileName into: dataStream [
	self openPassiveDataConnection.
	self sendCommand: 'RETR ', remoteFileName.
	[self checkResponse]
		on: TelnetProtocolError
		do: [:ex |
			self closeDataSocket.
			ex pass].
	self getDataInto: dataStream.
	self closeDataSocket.
	self checkResponse
]

{ #category : #protocol }
FTPClient >> getPartial: limit fileNamed: remoteFileName into: dataStream [
	| data |
	self openPassiveDataConnection.
	self sendCommand: 'RETR ', remoteFileName.
	[self checkResponse]
		on: TelnetProtocolError
		do: [:ex |
			self closeDataSocket.
			ex pass].
	data := self get: limit dataInto: dataStream.
	self abortDataConnection.
	^data

]

{ #category : #private }
FTPClient >> login [

	self user ifNil: [^self].

	["repeat both USER and PASS since some servers require it"
	self sendCommand: 'USER ', self user.

	"331 Password required"
	self lookForCode: 331.
	"will ask user, if needed"
	self sendCommand: 'PASS ', self password.

	"230 User logged in"
	([self lookForCode: 230.]
		on: TelnetProtocolError
		do: [false]) == false
		] whileTrue: [
			(LoginFailedException protocolInstance: self) signal: self lastResponse]


]

{ #category : #protocol }
FTPClient >> loginUser: userName password: passwdString [

	self user: userName.
	self password: passwdString.

	self login
]

{ #category : #'private protocol' }
FTPClient >> lookForCode: code ifDifferent: handleBlock [ 
	"We are expecting a certain numeric code next.  
	However, in the FTP protocol, multiple lines are allowed.  
	If the response is multi-line, the fourth character of the first line is a  
	$- and the last line repeats the numeric code but the code is followed by 
	a space. So it's possible that there are more lines left of the last response that 
	we need to throw away. We use peekForAll: so that we don't discard the
	next response that is not a continuation line."

	
	"check for multi-line response"
	(self lastResponse size > 3
			and: [(self lastResponse at: 4) = $-])
		ifTrue: ["Discard continuation lines."
			 | headToDiscard |
			headToDiscard := self lastResponse first: 4.
			[[self stream peekForAll: headToDiscard]
				whileTrue: [self stream nextLine]]
				on: Exception
				do: [:ex | ^handleBlock value: nil]].
	^ super lookForCode: code ifDifferent: handleBlock
]

{ #category : #protocol }
FTPClient >> makeDirectory: newDirName [
	self sendCommand: 'MKD ' , newDirName.
	self checkResponse.

]

{ #category : #protocol }
FTPClient >> openDataSocket: remoteHostAddress port: dataPort [
	dataSocket := Socket new.
	dataSocket connectTo: remoteHostAddress port: dataPort
]

{ #category : #'private protocol' }
FTPClient >> openPassiveDataConnection [
	| portInfo list dataPort remoteHostAddress remoteAddressString |
	self sendCommand: 'PASV'.
	self lookForCode: 227 ifDifferent: [:response | (TelnetProtocolError protocolInstance: self) signal: 'Could not enter passive mode: ' , response].

	portInfo := (self lastResponse findTokens: '()') at: 2.
	list := portInfo findTokens: ','.
	remoteHostAddress := ByteArray
		with: (list at: 1) asNumber
		with: (list at: 2) asNumber
		with: (list at: 3) asNumber
		with: (list at: 4) asNumber.
	remoteAddressString := String streamContents: [:addrStream | remoteHostAddress
		do: [ :each | each printOn: addrStream ]
		separatedBy: [ addrStream nextPut: $. ]].
 	dataPort := (list at: 5) asNumber * 256 + (list at: 6) asNumber.
	self openDataSocket: (NetNameResolver addressForName: remoteAddressString) port: dataPort
]

{ #category : #protocol }
FTPClient >> passive [
	self sendCommand: 'PASV'.
	self lookForCode: 227
]

{ #category : #protocol }
FTPClient >> putFileNamed: filePath as: fileNameOnServer [
	"FTP a file to the server."


	| fileStream |
	fileStream := FileStream readOnlyFileNamed: filePath.
	fileStream
		ifNil: [(FileDoesNotExistException fileName: filePath) signal].
	self putFileStreamContents: fileStream as: fileNameOnServer

]

{ #category : #protocol }
FTPClient >> putFileStreamContents: fileStream as: fileNameOnServer [
	"FTP a file to the server."


	self openPassiveDataConnection.
	self sendCommand: 'STOR ', fileNameOnServer.

	fileStream reset.

	[self sendStreamContents: fileStream]
		ensure: [self closeDataSocket].

	self checkResponse.
	self checkResponse.

]

{ #category : #protocol }
FTPClient >> pwd [
	| result |
	self sendCommand: 'PWD'.
	self lookForCode: 257.
	result := self lastResponse.
	^result copyFrom: (result indexOf: $")+1 to: (result lastIndexOf: $")-1
]

{ #category : #protocol }
FTPClient >> quit [
	self sendCommand: 'QUIT'.
	self close
]

{ #category : #protocol }
FTPClient >> removeFileNamed: remoteFileName [
	self sendCommand: 'DELE ', remoteFileName.
	self checkResponse.

]

{ #category : #protocol }
FTPClient >> renameFileNamed: oldFileName to: newFileName [
	self sendCommand: 'RNFR ' , oldFileName.
	self lookForCode: 350.
	self sendCommand: 'RNTO ' , newFileName.
	self lookForCode: 250
]

{ #category : #private }
FTPClient >> sendStreamContents: aStream [
	self dataSocket sendStreamContents: aStream checkBlock: [self checkForPendingError. true]
]
