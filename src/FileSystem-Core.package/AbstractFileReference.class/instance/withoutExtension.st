copying
withoutExtension
  "Returns the new reference based on receiver with fullname without its extension"

  ^  (self parent / self basenameWithoutExtension)