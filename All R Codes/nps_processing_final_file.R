library(hunspell)
library(spelling)
library(spellcheckr)
# Check individual words
words <- c("automatic", "wiskey", "aotomatic","supplier","supplyer")
correct <- hunspell_check(words)
print(correct)

hunspell(words)
