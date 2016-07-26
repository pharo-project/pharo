initialize-release
writePropertiesFile
  | properties |
  properties := Dictionary new.
  properties at: 'noMethodMetaData' put: true.
  properties at: 'separateMethodMetaAndSource' put: false.
  properties at: 'useCypressPropertiesFile' put: true.
  self
    writeInDirectoryName: '.'
    fileName: ''
    extension: '.filetree'
    visit: [ properties writeCypressJsonOn: fileStream ].
  self
    writeInDirectoryName: '.'
    fileName: 'properties'
    extension: self propertyFileExtension
    visit: [ Dictionary new writeCypressJsonOn: fileStream ]