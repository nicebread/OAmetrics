# Get network analysis of co-authorships for a given author or set of works

This function performs a network analysis for a given author or set of
works, providing indicators for the degree of international
collaborations.

## Usage

``` r
get_network(
  author.id,
  doi = NA,
  works = NA,
  min_coauthorships = 2,
  verbose = TRUE
)
```

## Arguments

- author.id:

  The ID of the author for whom the network will be analyzed. Must be
  provided.

- doi:

  Optional vector of DOIs. If provided, only works with these DOIs will
  be analyzed.

- works:

  Optional data frame of works (that have already been fetched by
  oa_fetch). Either provide dois or works.

- min_coauthorships:

  Minimal number of co-authorships. By default set to 2: This way,
  single co-authorship connections are ignored (e.g., in ManyLabs-style
  papers, this can lead to a strong inflation of co-authorship
  connections.)

- verbose:

  Whether to display the result string (default is TRUE).

## Value

A list containing various results of the network analysis, including
unique coauthor edges, counts of international and same-country
co-authors, country codes of coauthors, and a result string summarizing
the analysis. It also computes the normalized Shannon entropy. It is
normalized by dividing it by log_2(n_countries), which bounds its range
to \[0; 1\].

## Examples

``` r
# Analyze the network of a specific author
get_network(author.id = 'https://openalex.org/A5022479713')
#> Requesting url:
#> <https://api.openalex.org/works?filter=author.id%3Ahttps%3A%2F%2Fopenalex.org%2FA5022479713%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse>
#> ℹ Getting 2 pages of results with a total of 275 records...
#> ⠙ Converting [90/275] ■■■■■■■■■■■                       33% ETA:  2s
#> ⠹ Converting [240/275] ■■■■■■■■■■■■■■■■■■■■■■■■■■■       87% ETA:  0s
#> ⠹ Converting [275/275] ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% ETA:  0s
#> 
#> [1] "Using the provided works."
#> [1] "289 unique identifiable co-authors with at least 2 joint papers; 73% from 28 international countries, 27% from the same country. The evenness (ranging from 0=only one country to 1=even distribution among all countries) is 0.77. The 4 countries with the most coauthors are: DE (79), US (74), NL (43), GB (24)."
#> [1] "3 primary fields: Psychology (136), Decision Sciences (47), Computer Science (32). The evenness (ranging from 0=only one field to 1=even distribution among all fields) is 0.51."
#> $unique_coauthor_edges
#> # A tibble: 428 × 3
#>    id                               country_code n_coauthorships
#>    <chr>                            <chr>                  <int>
#>  1 https://openalex.org/A5056348040 DE                        18
#>  2 https://openalex.org/A5039784616 DE                        16
#>  3 https://openalex.org/A5063079579 NL                        16
#>  4 https://openalex.org/A5052525257 DE                        13
#>  5 https://openalex.org/A5031368517 DE                        12
#>  6 https://openalex.org/A5055189096 DE                        11
#>  7 https://openalex.org/A5083504666 DE                        11
#>  8 https://openalex.org/A5089676027 DE                        10
#>  9 https://openalex.org/A5000733234 DE                         9
#> 10 https://openalex.org/A5043481987 DE                         9
#> # ℹ 418 more rows
#> 
#> $n_coauthors_international
#> [1] 210
#> 
#> $n_coauthors_same_country
#> [1] 79
#> 
#> $international_evenness
#> [1] 0.7730475
#> 
#> $country_codes_repeated
#> # A tibble: 29 × 2
#>    country_code     n
#>    <chr>        <int>
#>  1 DE              79
#>  2 US              74
#>  3 NL              43
#>  4 GB              24
#>  5 IT               9
#>  6 AT               7
#>  7 CH               7
#>  8 SE               7
#>  9 AU               4
#> 10 ES               4
#> # ℹ 19 more rows
#> 
#> $internationalization_string
#> [1] "289 unique identifiable co-authors with at least 2 joint papers; 73% from 28 international countries, 27% from the same country. The evenness (ranging from 0=only one country to 1=even distribution among all countries) is 0.77. The 4 countries with the most coauthors are: DE (79), US (74), NL (43), GB (24)."
#> 
#> $interdisc_evenness
#> [1] 0.5054292
#> 
#> $primary_fields_tab
#>                                   primary_field   n
#> 1                                    Psychology 136
#> 2                             Decision Sciences  47
#> 3                              Computer Science  32
#> 4                            Health Professions   7
#> 5                           Arts and Humanities   5
#> 6                                   Mathematics   5
#> 7                                      Medicine   5
#> 8  Biochemistry, Genetics and Molecular Biology   3
#> 9           Economics, Econometrics and Finance   3
#> 10                                  Engineering   3
#> 11          Business, Management and Accounting   2
#> 12                                 Neuroscience   2
#> 13         Agricultural and Biological Sciences   1
#> 14                        Environmental Science   1
#> 15                        Physics and Astronomy   1
#> 
#> $primary_fields_tab_reduced
#>       primary_field   n
#> 1        Psychology 136
#> 2 Decision Sciences  47
#> 3  Computer Science  32
#> 
#> $subfields_tab
#>                                                 subfield  n
#> 1                  Experimental and Cognitive Psychology 86
#> 2                                      Social Psychology 84
#> 3                        Sociology and Political Science 44
#> 4                Statistics, Probability and Uncertainty 44
#> 5                                    Clinical Psychology 41
#> 6                                     Applied Psychology 38
#> 7                                    Information Systems 28
#> 8             Management Science and Operations Research 24
#> 9                     Information Systems and Management 23
#> 10                            Statistics and Probability 15
#> 11                                Cognitive Neuroscience 14
#> 12                  Computer Networks and Communications 12
#> 13                               Artificial Intelligence 10
#> 14                            General Health Professions  8
#> 15                                    General Psychology  8
#> 16                                                Health  7
#> 17              Developmental and Educational Psychology  6
#> 18                            Economics and Econometrics  6
#> 19                     History and Philosophy of Science  5
#> 20  Public Health, Environmental and Occupational Health  5
#> 21                     Statistical and Nonlinear Physics  5
#> 22                                Biomedical Engineering  3
#> 23                         Computer Science Applications  3
#> 24                                            Demography  3
#> 25                                          Food Science  3
#> 26                                     Molecular Biology  3
#> 27                                       Safety Research  3
#> 28                                         Communication  2
#> 29               Computer Vision and Pattern Recognition  2
#> 30                       Control and Systems Engineering  2
#> 31                                        Gender Studies  2
#> 32                               General Social Sciences  2
#> 33                                              Genetics  2
#> 34                        Management Information Systems  2
#> 35               Management of Technology and Innovation  2
#> 36                                   Medical Terminology  2
#> 37                                             Museology  2
#> 38                                          Pharmacology  2
#> 39               Radiology, Nuclear Medicine and Imaging  2
#> 40                                              Software  2
#> 41                               Strategy and Management  2
#> 42                                Automotive Engineering  1
#> 43                               Behavioral Neuroscience  1
#> 44                                          Biochemistry  1
#> 45                Cardiology and Cardiovascular Medicine  1
#> 46           Computer Graphics and Computer-Aided Design  1
#> 47          Ecology, Evolution, Behavior and Systematics  1
#> 48                                             Education  1
#> 49                                          Epidemiology  1
#> 50                             General Decision Sciences  1
#> 51           General Economics, Econometrics and Finance  1
#> 52                                   Infectious Diseases  1
#> 53                              Language and Linguistics  1
#> 54                        Literature and Literary Theory  1
#> 55                Management, Monitoring, Policy and Law  1
#> 56 Organizational Behavior and Human Resource Management  1
#> 57                                            Philosophy  1
#> 58         Political Science and International Relations  1
#> 59                    Pulmonary and Respiratory Medicine  1
#> 60                                 Reproductive Medicine  1
#> 61                                     Signal Processing  1
#> 
#> $topics_tab
#>                                                                  topic  n
#> 1                                        Mental Health Research Topics 51
#> 2                                 Attachment and Relationship Dynamics 25
#> 3                                  Behavioral Health and Interventions 23
#> 4                                 Meta-analysis and systematic reviews 23
#> 5                                   Research Data Management Practices 23
#> 6                                      Cultural Differences and Values 20
#> 7                           Evolutionary Psychology and Human Behavior 19
#> 8                             Scientific Computing and Data Management 18
#> 9                                     Social and Intergroup Psychology 18
#> 10                           scientometrics and bibliometrics research 17
#> 11                                   Personality Traits and Psychology 16
#> 12                            Advanced Statistical Modeling Techniques 12
#> 13                                  Death Anxiety and Social Exclusion 12
#> 14                                Psychological Testing and Assessment 12
#> 15                           Personality Disorders and Psychopathology 11
#> 16                  Academic and Historical Perspectives in Psychology  8
#> 17                                     Cognitive Abilities and Testing  8
#> 18                               Evaluation and Performance Assessment  8
#> 19                              Statistical Methods in Clinical Trials  8
#> 20                      Psychological Well-being and Life Satisfaction  7
#> 21                            Neural and Behavioral Psychology Studies  5
#> 22                                 Optimal Experimental Design Methods  5
#> 23                              Psychometric Methodologies and Testing  5
#> 24                             Advanced Statistical Methods and Models  4
#> 25                                 Complex Network Analysis Techniques  4
#> 26                                Counseling Practices and Supervision  4
#> 27                               Functional Brain Connectivity Studies  4
#> 28                                     Health disparities and outcomes  4
#> 29                           Psychotherapy Techniques and Applications  4
#> 30                           Qualitative Comparative Analysis Research  4
#> 31                                Biomedical and Engineering Education  3
#> 32                     Cognitive and psychological constructs research  3
#> 33                                    Crime Patterns and Interventions  3
#> 34                                                Data Analysis with R  3
#> 35                                        Delphi Technique in Research  3
#> 36                                   Family Dynamics and Relationships  3
#> 37                             Forecasting Techniques and Applications  3
#> 38                                   Philosophy and History of Science  3
#> 39                                   Psychology, Coaching, and Therapy  3
#> 40                              Religion, Spirituality, and Psychology  3
#> 41                            Sensory Analysis and Statistical Methods  3
#> 42                                        Technology and Data Analysis  3
#> 43                                          AI in Service Interactions  2
#> 44                             Advanced Statistical Process Monitoring  2
#> 45                                Behavioral and Psychological Studies  2
#> 46                                  Big Data and Business Intelligence  2
#> 47                                          Child Welfare and Adoption  2
#> 48         Child and Adolescent Psychosocial and Emotional Development  2
#> 49                             Computational and Text Analysis Methods  2
#> 50                           Criminal Justice and Corrections Analysis  2
#> 51                                         Data Quality and Management  2
#> 52                                    Data Visualization and Analytics  2
#> 53                                    Digital Innovation in Industries  2
#> 54                                 Digital Mental Health Interventions  2
#> 55                                  Educational Games and Gamification  2
#> 56                              Emotional Intelligence and Performance  2
#> 57                                         Emotions and Moral Behavior  2
#> 58                                         Ethics in Clinical Research  2
#> 59                                      Evolution and Genetic Dynamics  2
#> 60                                     Face Recognition and Perception  2
#> 61                   Genetics, Bioinformatics, and Biomedical Research  2
#> 62               Health Systems, Economic Evaluations, Quality of Life  2
#> 63                                 Health and Medical Research Impacts  2
#> 64                                          Health and Medical Studies  2
#> 65                                  Libraries and Information Services  2
#> 66                                      Medical Research and Practices  2
#> 67                                      Misinformation and Its Impacts  2
#> 68                             Psychiatry, Mental Health, Neuroscience  2
#> 69                    Psychological and Temporal Perspectives Research  2
#> 70                               Psychology Research and Bibliometrics  2
#> 71                          Psychology of Moral and Emotional Judgment  2
#> 72                  Psychopathy, Forensic Psychiatry, Sexual Offending  2
#> 73                            Reliability and Agreement in Measurement  2
#> 74                            Sexual Assault and Victimization Studies  2
#> 75                                    Social Robot Interaction and HRI  2
#> 76                                     Sociology and Education Studies  2
#> 77                                    Sports Analytics and Performance  2
#> 78                                        Sports Science and Education  2
#> 79                          Statistical Methods and Bayesian Inference  2
#> 80                                 Academic Publishing and Open Access  1
#> 81                           Adolescent Sexual and Reproductive Health  1
#> 82                               Advanced Malware Detection Techniques  1
#> 83                               Advances in Oncology and Radiotherapy  1
#> 84                                Aging, Elder Care, and Social Issues  1
#> 85  Anxiety, Depression, Psychometrics, Treatment, Cognitive Processes  1
#> 86                              Big Data Technologies and Applications  1
#> 87                                   Body Image and Dysmorphia Studies  1
#> 88                                          COVID-19 and Mental Health  1
#> 89                           Cardiac, Anesthesia and Surgical Outcomes  1
#> 90                                       Cognitive Science and Mapping  1
#> 91                                    Community Health and Development  1
#> 92                            Complex Systems and Time Series Analysis  1
#> 93                                   Conflict of Laws and Jurisdiction  1
#> 94                                        Corporate Governance and Law  1
#> 95                                 Corporate Governance and Management  1
#> 96                                         Data Analysis and Archiving  1
#> 97                             Data Mining Algorithms and Applications  1
#> 98                         Deception detection and forensic psychology  1
#> 99                            Decision-Making and Behavioral Economics  1
#> 100                                       Digital Media and Visual Art  1
#> 101             Diverse Approaches in Healthcare and Education Studies  1
#> 102                                E-Learning and Knowledge Management  1
#> 103                           Economic, financial, and policy analysis  1
#> 104                             Educational Assessment and Improvement  1
#> 105                          Educational Strategies and Epistemologies  1
#> 106                                       Emotion and Mood Recognition  1
#> 107                               Environmental Science and Technology  1
#> 108                                   Ethics in Business and Education  1
#> 109                                         Ethics in medical practice  1
#> 110                           Evolutionary Game Theory and Cooperation  1
#> 111                          Experimental Behavioral Economics Studies  1
#> 112                          Explainable Artificial Intelligence (XAI)  1
#> 113                                          Family Support in Illness  1
#> 114                             Family and Disability Support Research  1
#> 115                                Fault Detection and Control Systems  1
#> 116                                   Gene Regulatory Network Analysis  1
#> 117                              Grief, Bereavement, and Mental Health  1
#> 118                            Hate Speech and Cyberbullying Detection  1
#> 119                                       Health, Medicine and Society  1
#> 120                                 Health, psychology, and well-being  1
#> 121                               Healthcare Systems and Public Health  1
#> 122                                Hermeneutics and Narrative Identity  1
#> 123                                       Housing Market and Economics  1
#> 124                                         Human Motion and Animation  1
#> 125                               Human Resource and Talent Management  1
#> 126                                     Information and Cyber Security  1
#> 127                                  Infrared Thermography in Medicine  1
#> 128               Innovative Teaching Methodologies in Social Sciences  1
#> 129                   Intergenerational Family Dynamics and Caregiving  1
#> 130                                   Knowledge Management and Sharing  1
#> 131                                    Korean Urban and Social Studies  1
#> 132                           Leadership, Courage, and Heroism Studies  1
#> 133                                   Linguistic research and analysis  1
#> 134                                         Media Influence and Health  1
#> 135                              Mobile Crowdsensing and Crowdsourcing  1
#> 136                       Model-Driven Software Engineering Techniques  1
#> 137                              Motivation and Self-Concept in Sports  1
#> 138                            Musculoskeletal pain and rehabilitation  1
#> 139                                 Neural dynamics and brain function  1
#> 140                              Opinion Dynamics and Social Influence  1
#> 141                                     Optimism, Hope, and Well-being  1
#> 142                                 Paranormal Experiences and Beliefs  1
#> 143                  Personal Information Management and User Behavior  1
#> 144                                           Plant and animal studies  1
#> 145                        Psychiatric care and mental health services  1
#> 146                     Psychological and Educational Research Studies  1
#> 147                            Psychology of Development and Education  1
#> 148                                  Religion and Society Interactions  1
#> 149                                 Religion, Society, and Development  1
#> 150                               Reproductive Health and Technologies  1
#> 151                                   SARS-CoV-2 detection and testing  1
#> 152                                        Sex work and related issues  1
#> 153                                Sexuality, Behavior, and Technology  1
#> 154                                        Sleep and related disorders  1
#> 155                                        Social Capital and Networks  1
#> 156                                          Social Media and Politics  1
#> 157                                   Social Power and Status Dynamics  1
#> 158                          Software Testing and Debugging Techniques  1
#> 159                                   Spatial Cognition and Navigation  1
#> 160                               Statistical Methods and Applications  1
#> 161                                      Stress Responses and Cortisol  1
#> 162                                  Teaching and Learning Programming  1
#> 163                                      Treatment of Major Depression  1
#> 164                             Winter Sports Injuries and Performance  1
#> 165                              Youth Education and Societal Dynamics  1
#> 166                        demographic modeling and climate adaptation  1
#> 
#> $interdisc_string
#> [1] "3 primary fields: Psychology (136), Decision Sciences (47), Computer Science (32). The evenness (ranging from 0=only one field to 1=even distribution among all fields) is 0.51."
#> 
# Analyze the network of a specific author for selected works
get_network(author.id = 'https://openalex.org/A5022479713',
    doi = c(
        'https://doi.org/10.1037/pspp0000428',
        'https://doi.org/10.1017/S0033291722003294',
        'https://doi.org/10.5964/ps.6029',
        'https://doi.org/10.1146/annurev-psych-020821-114157'))
#> Requesting url:
#> <https://api.openalex.org/works?filter=doi%3Ahttps%3A%2F%2Fdoi.org%2F10.1037%2Fpspp0000428%7Chttps%3A%2F%2Fdoi.org%2F10.1017%2FS0033291722003294%7Chttps%3A%2F%2Fdoi.org%2F10.5964%2Fps.6029%7Chttps%3A%2F%2Fdoi.org%2F10.1146%2Fannurev-psych-020821-114157%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse>
#> ℹ Getting 1 page of results with a total of 4 records...
#> Warning: Only one country present, setting evenness index to 0.
#> [1] "0 unique identifiable co-authors with at least 2 joint papers; NaN% from -1 international countries, NaN% from the same country. The evenness (ranging from 0=only one country to 1=even distribution among all countries) is 0."
#> Warning: Only one field present, setting evenness index to 0.
#> [1] "1 primary fields: Psychology (4). The evenness (ranging from 0=only one field to 1=even distribution among all fields) is 0."
#> $unique_coauthor_edges
#> # A tibble: 35 × 3
#>    id                               country_code n_coauthorships
#>    <chr>                            <chr>                  <int>
#>  1 https://openalex.org/A5001668502 DE                         1
#>  2 https://openalex.org/A5004866926 NL                         1
#>  3 https://openalex.org/A5006406097 AU                         1
#>  4 https://openalex.org/A5011404541 AU                         1
#>  5 https://openalex.org/A5015649291 NL                         1
#>  6 https://openalex.org/A5017494791 DE                         1
#>  7 https://openalex.org/A5021314811 SE                         1
#>  8 https://openalex.org/A5027185097 US                         1
#>  9 https://openalex.org/A5027692708 DE                         1
#> 10 https://openalex.org/A5027778611 DE                         1
#> # ℹ 25 more rows
#> 
#> $n_coauthors_international
#> [1] 0
#> 
#> $n_coauthors_same_country
#> [1] 0
#> 
#> $international_evenness
#> [1] 0
#> 
#> $country_codes_repeated
#> # A tibble: 0 × 2
#> # ℹ 2 variables: country_code <chr>, n <int>
#> 
#> $internationalization_string
#> [1] "0 unique identifiable co-authors with at least 2 joint papers; NaN% from -1 international countries, NaN% from the same country. The evenness (ranging from 0=only one country to 1=even distribution among all countries) is 0."
#> 
#> $interdisc_evenness
#> [1] 0
#> 
#> $primary_fields_tab
#>   primary_field n
#> 1    Psychology 4
#> 
#> $primary_fields_tab_reduced
#>   primary_field n
#> 1    Psychology 4
#> 
#> $subfields_tab
#>                                  subfield n
#> 1   Experimental and Cognitive Psychology 5
#> 2 Statistics, Probability and Uncertainty 2
#> 3                      Applied Psychology 1
#> 4                  Cognitive Neuroscience 1
#> 5    Computer Networks and Communications 1
#> 6         Sociology and Political Science 1
#> 
#> $topics_tab
#>                                                                topic n
#> 1                                      Mental Health Research Topics 4
#> 2                               Meta-analysis and systematic reviews 2
#> 3                           Advanced Statistical Modeling Techniques 1
#> 4 Anxiety, Depression, Psychometrics, Treatment, Cognitive Processes 1
#> 5                                Digital Mental Health Interventions 1
#> 6                              Functional Brain Connectivity Studies 1
#> 7                          Qualitative Comparative Analysis Research 1
#> 
#> $interdisc_string
#> [1] "1 primary fields: Psychology (4). The evenness (ranging from 0=only one field to 1=even distribution among all fields) is 0."
#> 
```
