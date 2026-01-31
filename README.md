# utl-altair-slc-natural-language-processing-nlp-in-r-identify-nouns-pronouns-adjectives-text-ai
Altair slc natural language processing nlp in r identify nouns pronouns adjectives text ai
    %let pgm=utl-altair-slc-natural-language-processing-nlp-in-r-identify-nouns-pronouns-adjectives-ai-text;

    %stop_submission

    Altair slc natural language processing nlp in r identify nouns pronouns adjectives ai text

    Too long to post here, see github
    https://github.com/rogerjdeangelis/utl-altair-slc-natural-language-processing-nlp-in-r-identify-nouns-pronouns-adjectives-text-ai

    METHOD
    ------

    An annotator inheriting from classes
    Simple_POS_Tag_Annotator Annotator with description
    Computes POS tag annotations using the Apache OpenNLP Maxent
    Part of Speech tagger employing the default model for language 'en'

    INPUT TEXT
    ----------
      Pierre Vinken, 61 years old, will join the board as a nonexecutive director
      Nov. 29.\n Mr. Vinken is chairman of Elsevier N.V., the Dutch publishing group.

    OUTPUT
    ------
      WORK.WANT total obs=31

      WORD         GRAMMER                                     WORD       GRAMMER

      Pierre        Proper noun, singular                      Nov.        Proper noun, singular
      Vinken        Proper noun, singular                      29          Cardinal number
      ,             Punctuation                                .           Punctuation
      61            Cardinal number                            Mr.         Proper noun, singular
      years         Noun, plural                               Vinken      Proper noun, singular
      old           Adjective                                  is          Verb, 3rd person singular present
      ,             Punctuation                                chairman    Noun, singular or mass
      will          Modal                                      of          Preposition or subordinating conjunction
      join          Verb, base form                            Elsevier    Proper noun, singular
      the           Determiner                                 N.V.        Proper noun, singular
      board         Noun, singular or mass                     ,           Punctuation
      as            Preposition or subordinating conjunction   the         Determiner
      a             Determiner                                 Dutch       Adjective
      nonexecutive  Adjective                                  publishing  Noun, singular or mass
      director      Noun, singular or mass                     group       Noun, singular or mass

    stackoverflow
    https://stackoverflow.com/questions/51861346/r-pos-tagging-and-tokenizing-in-one-go

    PREP
    ---
    Installing Java (without paying oracle)

        1 Close everthingincluding windows explorer
        2 removed and reinstalled T package rJava if installed
        3 Close everthingincluding windows explorer
        4 installed Eclipse AdoptiumJava (should set path but you may want to check?)
        5 I rebooted (you may not need to do this if you have refreshenv command or
          restart windows explorer from task manager?)
        6 opened ultraEedit
        7 There are quiute a few R and Pyhton packages that require Java?

    /*                   _
    (_)_ __  _ __  _   _| |_
    | | `_ \| `_ \| | | | __|
    | | | | | |_) | |_| | |_
    |_|_| |_| .__/ \__,_|\__|
            |_|
    */

     options validvarname=upcase;

     data workx.txt;
       txt=compbl('Pierre Vinken, 61 years old, will join the board as a nonexecutive director:
            Nov. 29. Mr. Vinken; is chairman of Elsevier N.V., the Dutch publishing group?');
     run;

    /*
     _ __  _ __ ___   ___ ___  ___
    | `_ \| `__/ _ \ / __/ _ \/ __|
    | |_) | | | (_) | (_|  __/\__ \
    | .__/|_|  \___/ \___\___||___/
    |_|
    */

     libname otw wpd "d:/wpswrkx";
    options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
    proc r;
    export r=txt data=workx.txt;
    submit;
    library(rJava)
    library(stringr)
    library(NLP)
    library(openNLP)
    s <- as.String(txt$TXT)
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    word_token_annotator <- Maxent_Word_Token_Annotator()
    a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
    pos_tag_annotator <- Maxent_POS_Tag_Annotator()
    pos_tag_annotator
    a3 <- annotate(s, pos_tag_annotator, a2)
    a3
    head(annotate(s, Maxent_POS_Tag_Annotator(probs = TRUE), a2))
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, `[[`, "POS")
    tags
    table(tags)
    want<-as.data.frame(sprintf("%s/%s", s[a3w], tags))
    colnames(want)<-"wrdGrm"
    str(want)
    endsubmit;
    import r=want data=otw.want;
    run;

    proc format ;
     value $tok2grammer
        "CC  "="Coordinating conjunction                "
        "CD  "="Cardinal number                         "
        "DT  "="Determiner                              "
        "EX  "="Existential there                       "
        "FW  "="Foreign word                            "
        "IN  "="Preposition or subordinating conjunction"
        "JJ  "="Adjective                               "
        "JJR "="Adjective, comparative                  "
        "JJS "="Adjective, superlative                  "
        "LS  "="List item marker                        "
        "MD  "="Modal                                   "
        "NN  "="Noun, singular or mass                  "
        "NNS "="Noun, plural                            "
        "NNP "="Proper noun, singular                   "
        "NNPS"="Proper noun, plural                     "
        "PDT "="Predeterminer                           "
        "POS "="Possessive ending                       "
        "PRP "="Personal pronoun                        "
        "PRP$"="Possessive pronoun                      "
        "RB  "="Adverb                                  "
        "RBR "="Adverb, comparative                     "
        "RBS "="Adverb, superlative                     "
        "RP  "="Particle                                "
        "SYM "="Symbol                                  "
        "UH  "="Interjection                            "
        "VB  "="Verb, base form                         "
        "VBD "="Verb, past tense                        "
        "VBG "="Verb , gerund or present participle     "
        "VBN "="Verb, past participle                   "
        "VBP "="Verb, non­3rd person singular present   "
        "VBZ "="Verb, 3rd person singular present       "
        "WDT "="Wh­determiner                           "
        "WP  "="Wh­pronoun                              "
        "WP$ "="Possessive wh­pronoun                   "
        "WRB "="Wh­adverb                               "
        other="Symbol/Punctuation                       "
    ;run;quit;

    proc format cntlout=x;
     select $tok2grammer;
    run;quit;

    libname otw wpd "d:/wpswrkx";
    data workx.tags;
      set otw.want;
      word=scan(wrdGrm,1,'/');
      grm=scan(wrdGrm,2,'/');
      grammer=put(strip(grm),$tok2grammer.);
      if word in (  ','
                 , '.'
                 , '?'
                 , '!'
                 , ';'
                 , ':') then do; grm='PNC';grammer="Punctuation";end;
      keep word grammer;
    run;quit;

    proc print data=workx.tags;
    run;quit;

    /*           _               _
      ___  _   _| |_ _ __  _   _| |_
     / _ \| | | | __| `_ \| | | | __|
    | (_) | |_| | |_| |_) | |_| | |_
     \___/ \__,_|\__| .__/ \__,_|\__|
                    |_|
    */

    /**************************************************************************************************************************/
    /*  Altair SLC                                                                                                            */
    /*                                                                                                                        */
    /*  Obs        WORD                        GRAMMER                                                                        */
    /*                                                                                                                        */
    /*    1    Pierre          Proper noun, singular                                                                          */
    /*    2    Vinken          Proper noun, singular                                                                          */
    /*    3    ,               Punctuation                                                                                    */
    /*    4    61              Cardinal number                                                                                */
    /*    5    years           Noun, plural                                                                                   */
    /*    6    old             Adjective                                                                                      */
    /*    7    ,               Punctuation                                                                                    */
    /*    8    will            Modal                                                                                          */
    /*    9    join            Verb, base form                                                                                */
    /*   10    the             Determiner                                                                                     */
    /*   11    board           Noun, singular or mass                                                                         */
    /*   12    as              Preposition or subordinating conjunction                                                       */
    /*   13    a               Determiner                                                                                     */
    /*   14    nonexecutive    Adjective                                                                                      */
    /*   15    director        Noun, singular or mass                                                                         */
    /*   16    :               Punctuation                                                                                    */
    /*   17    Nov.            Proper noun, singular                                                                          */
    /*   18    29              Cardinal number                                                                                */
    /*   19    .               Punctuation                                                                                    */
    /*   20    Mr.             Proper noun, singular                                                                          */
    /*   21    Vinken          Proper noun, singular                                                                          */
    /*   22    ;               Punctuation                                                                                    */
    /*   23    is              Verb, 3rd person singular present                                                              */
    /*   24    chairman        Noun, singular or mass                                                                         */
    /*   25    of              Preposition or subordinating conjunction                                                       */
    /*   26    Elsevier        Proper noun, singular                                                                          */
    /*   27    N.V.            Proper noun, singular                                                                          */
    /*   28    ,               Punctuation                                                                                    */
    /*   29    the             Determiner                                                                                     */
    /*   30    Dutch           Adjective                                                                                      */
    /*   31    publishing      Noun, singular or mass                                                                         */
    /*   32    group           Noun, singular or mass                                                                         */
    /*   33    ?               Punctuation                                                                                    */
    /*                                                                                                                        */
    /*  An annotator inheriting from classes                                                                                  */
    /*    Simple_POS_Tag_Annotator Annotator                                                                                  */
    /*    with description                                                                                                    */
    /*    Computes POS tag annotations using the Apache OpenNLP Maxent Part of                                                */
    /*  Speech tagger employing the default model for language 'en'                                                           */
    /*   id type     start end features                                                                                       */
    /*    1 sentence     1  85 constituents=<<integer,19>>                                                                    */
    /*    2 sentence    87 155 constituents=<<integer,14>>                                                                    */
    /*    3 word         1   6 POS=NNP                                                                                        */
    /*    4 word         8  13 POS=NNP                                                                                        */
    /*    5 word        14  14 POS=,                                                                                          */
    /*    6 word        16  17 POS=CD                                                                                         */
    /*    7 word        19  23 POS=NNS                                                                                        */
    /*    8 word        25  27 POS=JJ                                                                                         */
    /*    9 word        28  28 POS=,                                                                                          */
    /*   10 word        30  33 POS=MD                                                                                         */
    /*   11 word        35  38 POS=VB                                                                                         */
    /*   12 word        40  42 POS=DT                                                                                         */
    /*   13 word        44  48 POS=NN                                                                                         */
    /*   14 word        50  51 POS=IN                                                                                         */
    /*   15 word        53  53 POS=DT                                                                                         */
    /*   16 word        55  66 POS=JJ                                                                                         */
    /*   17 word        68  75 POS=NN                                                                                         */
    /*   18 word        76  76 POS=:                                                                                          */
    /*   19 word        78  81 POS=NNP                                                                                        */
    /*   20 word        83  84 POS=CD                                                                                         */
    /*   21 word        85  85 POS=.                                                                                          */
    /*   22 word        87  89 POS=NNP                                                                                        */
    /*   23 word        91  96 POS=NNP                                                                                        */
    /*   24 word        97  97 POS=:                                                                                          */
    /*   25 word        99 100 POS=VBZ                                                                                        */
    /*   26 word       102 109 POS=NN                                                                                         */
    /*   27 word       111 112 POS=IN                                                                                         */
    /*   28 word       114 121 POS=NNP                                                                                        */
    /*   29 word       123 126 POS=NNP                                                                                        */
    /*   30 word       127 127 POS=,                                                                                          */
    /*   31 word       129 131 POS=DT                                                                                         */
    /*   32 word       133 137 POS=JJ                                                                                         */
    /*   33 word       139 148 POS=NN                                                                                         */
    /*   34 word       150 154 POS=NN                                                                                         */
    /*   35 word       155 155 POS=.                                                                                          */
    /*                                                                                                                        */
    /*   id type     start end features                                                                                       */
    /*    1 sentence     1  85 constituents=<<integer,19>>                                                                    */
    /*    2 sentence    87 155 constituents=<<integer,14>>                                                                    */
    /*    3 word         1   6 POS=NNP, POS_prob=0.9476405                                                                    */
    /*    4 word         8  13 POS=NNP, POS_prob=0.9692841                                                                    */
    /*    5 word        14  14 POS=,, POS_prob=0.9884445                                                                      */
    /*    6 word        16  17 POS=CD, POS_prob=0.9926943                                                                     */
    /*                                                                                                                        */
    /*   [1] "NNP" "NNP" ","   "CD"  "NNS" "JJ"  ","   "MD"  "VB"  "DT"  "NN"  "IN"  "DT"                                     */
    /*  "JJ"  "NN"  ":"   "NNP" "CD"  "."   "NNP" "NNP" ":"   "VBZ" "NN"  "IN"  "NNP"                                         */
    /*  "NNP" ","   "DT"  "JJ"  "NN"  "NN"  "."                                                                               */
    /*                                                                                                                        */
    /*  tags                                                                                                                  */
    /*    ,   .   :  CD  DT  IN  JJ  MD  NN NNP NNS  VB VBZ                                                                   */
    /*    3   2   2   2   3   2   3   1   5   7   1   1   1                                                                   */
    /*                                                                                                                        */
    /*  'data.frame':      33 obs. of  1 variable:                                                                            */
    /*   $ wrdGrm: chr  "Pierre/NNP" "Vinken/NNP" ",/," "61/CD" ...                                                           */
    /*                                                                                                                        */
    /*  Altair SLC                                                                                                            */
    /*                                                                                                                        */
    /*  Obs        WORD                        GRAMMER                                                                        */
    /*                                                                                                                        */
    /*    1    Pierre          Proper noun, singular                                                                          */
    /*    2    Vinken          Proper noun, singular                                                                          */
    /*    3    ,               Punctuation                                                                                    */
    /*    4    61              Cardinal number                                                                                */
    /*    5    years           Noun, plural                                                                                   */
    /*    6    old             Adjective                                                                                      */
    /*    7    ,               Punctuation                                                                                    */
    /*    8    will            Modal                                                                                          */
    /*    9    join            Verb, base form                                                                                */
    /*   10    the             Determiner                                                                                     */
    /*   11    board           Noun, singular or mass                                                                         */
    /*   12    as              Preposition or subordinating conjunction                                                       */
    /*   13    a               Determiner                                                                                     */
    /*   14    nonexecutive    Adjective                                                                                      */
    /*   15    director        Noun, singular or mass                                                                         */
    /*   16    :               Punctuation                                                                                    */
    /*   17    Nov.            Proper noun, singular                                                                          */
    /*   18    29              Cardinal number                                                                                */
    /*   19    .               Punctuation                                                                                    */
    /*   20    Mr.             Proper noun, singular                                                                          */
    /*   21    Vinken          Proper noun, singular                                                                          */
    /*   22    ;               Punctuation                                                                                    */
    /*   23    is              Verb, 3rd person singular present                                                              */
    /*   24    chairman        Noun, singular or mass                                                                         */
    /*   25    of              Preposition or subordinating conjunction                                                       */
    /*   26    Elsevier        Proper noun, singular                                                                          */
    /*   27    N.V.            Proper noun, singular                                                                          */
    /*   28    ,               Punctuation                                                                                    */
    /*   29    the             Determiner                                                                                     */
    /*   30    Dutch           Adjective                                                                                      */
    /*   31    publishing      Noun, singular or mass                                                                         */
    /*   32    group           Noun, singular or mass                                                                         */
    /*   33    ?               Punctuation                                                                                    */
    /**************************************************************************************************************************/

    /*
    | | ___   __ _
    | |/ _ \ / _` |
    | | (_) | (_| |
    |_|\___/ \__, |
             |___/
    */

    1                                          Altair SLC      10:13 Saturday, January 31, 2026

    NOTE: Copyright 2002-2025 World Programming, an Altair Company
    NOTE: Altair SLC 2026 (05.26.01.00.000758)
          Licensed to Roger DeAngelis
    NOTE: This session is executing on the X64_WIN11PRO platform and is running in 64 bit mode

    NOTE: AUTOEXEC processing beginning; file is C:\wpsoto\autoexec.sas
    NOTE: AUTOEXEC source line
    1       +  ï»¿ods _all_ close;
               ^
    ERROR: Expected a statement keyword : found "?"
    NOTE: Library workx assigned as follows:
          Engine:        SAS7BDAT
          Physical Name: d:\wpswrkx

    NOTE: Library slchelp assigned as follows:
          Engine:        WPD
          Physical Name: C:\Progra~1\Altair\SLC\2026\sashelp


    LOG:  10:13:30
    NOTE: 1 record was written to file PRINT

    NOTE: The data step took :
          real time : 0.015
          cpu time  : 0.015


    NOTE: AUTOEXEC processing completed

    1          libname otw wpd "d:/wpswrkx";
    NOTE: Library otw assigned as follows:
          Engine:        WPD
          Physical Name: d:\wpswrkx

    2         options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
    3         proc r;
    NOTE: Using R version 4.5.2 (2025-10-31 ucrt) from C:\Program Files\R\R-4.5.2
    4         export r=txt data=workx.txt;
    NOTE: Creating R data frame 'txt' from data set 'WORKX.txt'

    5         submit;
    6         library(rJava)
    7         library(stringr)
    8         library(NLP)
    9         library(openNLP)
    10        s <- as.String(txt$TXT)
    11        sent_token_annotator <- Maxent_Sent_Token_Annotator()
    12        word_token_annotator <- Maxent_Word_Token_Annotator()
    13        a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
    14        pos_tag_annotator <- Maxent_POS_Tag_Annotator()
    15        pos_tag_annotator
    16        a3 <- annotate(s, pos_tag_annotator, a2)
    17        a3
    18        head(annotate(s, Maxent_POS_Tag_Annotator(probs = TRUE), a2))
    19        a3w <- subset(a3, type == "word")
    20        tags <- sapply(a3w$features, `[[`, "POS")
    21        tags
    22        table(tags)
    23        want<-as.data.frame(sprintf("%s/%s", s[a3w], tags))
    24        colnames(want)<-"wrdGrm"
    25        str(want)
    26        endsubmit;


    2                                                                                                                         Altair SLC

    NOTE: Submitting statements to R:

    > library(rJava)
    > library(stringr)
    > library(NLP)
    > library(openNLP)
    > s <- as.String(txt$TXT)
    > sent_token_annotator <- Maxent_Sent_Token_Annotator()
    > word_token_annotator <- Maxent_Word_Token_Annotator()
    > a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
    > pos_tag_annotator <- Maxent_POS_Tag_Annotator()
    > pos_tag_annotator
    > a3 <- annotate(s, pos_tag_annotator, a2)
    > a3
    > head(annotate(s, Maxent_POS_Tag_Annotator(probs = TRUE), a2))
    > a3w <- subset(a3, type == "word")
    > tags <- sapply(a3w$features, `[[`, "POS")
    > tags
    > table(tags)
    > want<-as.data.frame(sprintf("%s/%s", s[a3w], tags))
    > colnames(want)<-"wrdGrm"
    > str(want)

    NOTE: Processing of R statements complete

    27        import r=want data=otw.want;
    NOTE: Creating data set 'OTW.want' from R data frame 'want'
    NOTE: Column names modified during import of 'want'
    NOTE: Data set "OTW.want" has 33 observation(s) and 1 variable(s)

    28        run;
    NOTE: Procedure r step took :
          real time : 2.745
          cpu time  : 0.015


    29
    30
    31         proc format ;
    32          value $tok2grammer
    33             "CC  "="Coordinating conjunction                "
    34             "CD  "="Cardinal number                         "
    35             "DT  "="Determiner                              "
    36             "EX  "="Existential there                       "
    37             "FW  "="Foreign word                            "
    38             "IN  "="Preposition or subordinating conjunction"
    39             "JJ  "="Adjective                               "
    40             "JJR "="Adjective, comparative                  "
    41             "JJS "="Adjective, superlative                  "
    42             "LS  "="List item marker                        "
    43             "MD  "="Modal                                   "
    44             "NN  "="Noun, singular or mass                  "
    45             "NNS "="Noun, plural                            "
    46             "NNP "="Proper noun, singular                   "
    47             "NNPS"="Proper noun, plural                     "
    48             "PDT "="Predeterminer                           "
    49             "POS "="Possessive ending                       "
    50             "PRP "="Personal pronoun                        "
    51             "PRP$"="Possessive pronoun                      "
    52             "RB  "="Adverb                                  "
    53             "RBR "="Adverb, comparative                     "
    54             "RBS "="Adverb, superlative                     "
    55             "RP  "="Particle                                "

    3                                                                                                                         Altair SLC

    56             "SYM "="Symbol                                  "
    57             "UH  "="Interjection                            "
    58             "VB  "="Verb, base form                         "
    59             "VBD "="Verb, past tense                        "
    60             "VBG "="Verb , gerund or present participle     "
    61             "VBN "="Verb, past participle                   "
    62             "VBP "="Verb, non?3rd person singular present   "
    63             "VBZ "="Verb, 3rd person singular present       "
    64             "WDT "="Wh?determiner                           "
    65             "WP  "="Wh?pronoun                              "
    66             "WP$ "="Possessive wh?pronoun                   "
    67             "WRB "="Wh?adverb                               "
    68             other="Symbol/Punctuation                       "
    69         ;run;quit;
    NOTE: Format $tok2grammer output
    NOTE: Procedure format step took :
          real time : 0.016
          cpu time  : 0.000


    70
    71        proc format cntlout=x;
    72         select $tok2grammer;
    73        run;quit;
    NOTE: Data set "WORK.x" has 36 observation(s) and 21 variable(s)
    NOTE: Procedure format step took :
          real time : 0.016
          cpu time  : 0.015


    74
    75        libname otw wpd "d:/wpswrkx";
    NOTE: Library otw assigned as follows:
          Engine:        WPD
          Physical Name: d:\wpswrkx

    76        data workx.tags;
    77          set otw.want;
    78          word=scan(wrdGrm,1,'/');
    79          grm=scan(wrdGrm,2,'/');
    80          grammer=put(strip(grm),$tok2grammer.);
    81          if word in (  ','
    82                     , '.'
    83                     , '?'
    84                     , '!'
    85                     , ';'
    86                     , ':') then do; grm='PNC';grammer="Punctuation";end;
    87          keep word grammer;
    88        run;

    NOTE: 33 observations were read from "OTW.want"
    NOTE: Data set "WORKX.tags" has 33 observation(s) and 2 variable(s)
    NOTE: The data step took :
          real time : 0.015
          cpu time  : 0.000


    88      !     quit;
    89
    90        proc print data=workx.tags;
    91        run;quit;
    NOTE: 33 observations were read from "WORKX.tags"
    NOTE: Procedure print step took :

    4                                                                                                                         Altair SLC

          real time : 0.015
          cpu time  : 0.000


    92
    ERROR: Error printed on page 1

    NOTE: Submitted statements took :
          real time : 2.872
          cpu time  : 0.093

    /*              _
      ___ _ __   __| |
     / _ \ `_ \ / _` |
    |  __/ | | | (_| |
     \___|_| |_|\__,_|

    */
