using TextAnalysis, JSON #, MultivariateStats, Clustering
using DataFrames

# Read in the text file

function read_json(file)
    open(file, "r") do f
        global inDict
        inDict = JSON.parse(f)
    end
    return inDict
end

textPath = string(pwd(), "/texts/isaiah.json")

isaiahjson = read_json(textPath)

isaiahtext = isaiahjson["text"]

# Want array as unified text for TextAnalysis
blockstring = ""
#for chapter in 1:length(isaiahtext)
for chapter in 24:27
    for verse in 1:length(isaiahtext[chapter])
        # print(string(chapter,":",verse," "))
        # print(isaiahtext[chapter][verse])
        global blockstring = string(blockstring," ",isaiahtext[chapter][verse])
    end
end

# Create a Corpus to Analyze

strgdc = StringDocument(blockstring)

# See the token before the cleaing
tokens(strgdc)

prepare!(strgdc, strip_punctuation)
prepare!(strgdc, strip_stopwords)
prepare!(strgdc, strip_case)
stem!(strgdc)

tokens(strgdc) # Now see it after the cleaning

ngrmdc = NGramDocument(blockstring)

crps = Corpus([strgdc])

lexicon(crps)
update_lexicon!(crps)
a = lexicon(crps)

# Get a DF for analysis and exploration
df_a = convert(DataFrame, a)

# Pivot to be more readable
colnames=names(df_a)
df_a[!, :id] = 1:nrow(df_a)
df_a = stack(df_a, colnames)

# Sort Descending
sort!(df_a, [:value], rev=true)

standardize!(crps, StringDocument)

update_lexicon!(crps)
update_inverse_index!(crps)

m = DocumentTermMatrix(crps)

dtm(m, :dense)