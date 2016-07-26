reporting
reportLinuxOSDetails: aStream
	self header: 'Operating System Details' on: aStream.
	#(
		'/etc/issue'
		'/etc/lsb-release'
		'/proc/version'
	) do: [:path|
		self writeContentsSafelyFromFile: path on: aStream]