scripting
projectForScriptEngine: aMetacelloScriptEngine unconditionalLoad: aBool
  "see https://github.com/dalehenrich/metacello-work/issues/244 ... uncoditionally load
   baseline"

  ^ aMetacelloScriptEngine getBaselineProjectUnconditionalLoad: true