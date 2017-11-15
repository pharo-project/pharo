
| dataFilePath |

dataFilePath := CommandLineArguments new arguments last.


CombinedChar parseCompositionMappingFrom: (ZnCharacterReadStream on: (File named: dataFilePath) readStream encoding: 'utf8').