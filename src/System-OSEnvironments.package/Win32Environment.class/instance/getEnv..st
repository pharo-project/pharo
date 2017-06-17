accessing
getEnv: aVariableName
	
	<todo>
	"The primitive on Windows is currently broken (2017-08-05) and instead of failing it can return nil.
	In such chase try to get value of the environment variable using FFI."

    | result | 
    
    result := super getEnv: aVariableName.
    ^ result ifNil: [self getEnvViaFFI: aVariableName ]