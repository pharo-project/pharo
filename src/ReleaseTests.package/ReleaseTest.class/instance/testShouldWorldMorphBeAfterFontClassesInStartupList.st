testing
testShouldWorldMorphBeAfterFontClassesInStartupList
	"This test documents issue https://pharo.fogbugz.com/f/cases/17834/World-Morph-is-started-up-before-Fonts"
	| startupList |
	
	startupList := (SessionManager default startupList collect: #handledId).
	
	#(StrikeFont LogicalFont FreeTypeSettings FreeTypeCache FT2Handle) 
		do: [ :fontClass |
			self should: [ (startupList indexOf: #WorldMorph) > (startupList indexOf: #SystemSettingsPersistence) ] ]