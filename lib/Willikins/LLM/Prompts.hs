module Willikins.LLM.Prompts where

-- | Generate a summary of the events of the day and the near future.
dailyBriefing :: String
dailyBriefing = unlines
  [ "It is your duty to provide a briefing summarising important information for the day."
  , "The briefing should have the following sections:"
  , ""
  , "Begin with a formal greeting."
  , ""
  , "**Today**"
  , ""
  , "Note any reminders about today's affairs."
  , ""
  , "**Looking Ahead**"
  , ""
  , "Offer a brief overview of forthcoming appointments, engagements, and tasks for the remainder of the week."
  , "Pay particular attention to tomorrow's schedule."
  , "You do not need to mention when I will be working from home, but do mention if I will be working from the office, or not working at all (apart from weekends when it is expected to not work)"
  , "If I am going to be on call, mention that a week in advance."
  ]

-- | Summarise an article.
summariseArticle :: String -> String
summariseArticle text = unlines
  [ "Give a short summary of the following article, a single paragraph pf 4 or 5 sentences at most."
  , "Do not include any preamble or suffix, just the summary paragraph and nothing else."
  , "If the article has any HTML or XML tags in don't mention them in your summary."
  , "If the article is incomplete, don't mention that, just summarise what's there."
  , ""
  , "Here's the content of the article:"
  , ""
  ] ++ text
