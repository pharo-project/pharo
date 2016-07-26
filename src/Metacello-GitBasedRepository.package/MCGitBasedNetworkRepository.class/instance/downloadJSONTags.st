private
downloadJSONTags
  | tagsUrl jsonObject |
  tagsUrl := self projectTagsUrlFor: self projectPath.
  jsonObject := MetacelloPlatform current
    downloadJSON: tagsUrl
    username: self class siteUsername
    pass: self class sitePassword.
  ^ self normalizeTagsData: jsonObject