variables initialization
updatePlatformInfo
	"Updates this shared pool with the info of the current platform."
	platformName := self currentPlatformName.
	wordSize := self currentWordSize.