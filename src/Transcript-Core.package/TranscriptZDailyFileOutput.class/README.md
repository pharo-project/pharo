TranscriptDailyFileOutput is a FileOutput type which appends the current date to the file name when it opens the file stream. The stream will automatically create a new filestream if the date set by the old one is not equal to the date today.

Detail:
#fileName returns the base filename for the file without any date set, if you want to know what the file that is created is called then use #datedFileName.

