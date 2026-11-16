from docx import Document
from docx.shared import Inches, Pt, RGBColor

d = Document()
s = d.sections[0]
s.top_margin = s.bottom_margin = Inches(.65)
s.left_margin = s.right_margin = Inches(.75)
s.page_width, s.page_height = Inches(8.5), Inches(11)
for name in ['Normal', 'Title', 'Heading 1', 'Heading 2', 'List Bullet']:
    st = d.styles[name]
    st.font.name = 'Calibri'
    st.font.color.rgb = RGBColor(0,0,0)
normal = d.styles['Normal']
normal.font.size = Pt(11)
normal.paragraph_format.space_after = Pt(6)
normal.paragraph_format.line_spacing = 1.03
d.styles['Title'].font.size = Pt(20)
d.styles['Title'].paragraph_format.space_after = Pt(9)
d.styles['Heading 1'].font.size = Pt(13)
d.styles['Heading 1'].paragraph_format.space_before = Pt(10)
d.styles['Heading 1'].paragraph_format.space_after = Pt(5)
def p(t): d.add_paragraph(t)
def h(t): d.add_heading(t,1)
def b(t): d.add_paragraph(t,'List Bullet')

d.add_paragraph('Project 2', 'Title')
d.add_paragraph('Bayesian Models of College Football Team Strength in 2026', 'Heading 1')
h('Overview')
p('Develop a Bayesian model to estimate college football team strength during the 2026 season. Use game results to compare teams while accounting for the opponents they face and relevant game conditions. Your analysis should quantify uncertainty in team strength and use the posterior distribution to predict games and simulate season outcomes.')
p('You may choose any suitable Bayesian modeling approach. A Bradley–Terry model is one option, but it is not required. Your model must connect team strength to game outcomes and produce posterior estimates and predictive probabilities.')
h('Data')
p('Use game-level college football data from the CollegeFootballData API, accessed through cfbfastR in R, or another documented source with comparable coverage. Include FBS and FCS teams and retain the games needed to estimate their strengths.')
b('Collect team identifiers, scores or win–loss outcomes, home and away designations, neutral-site indicators, season, and week. Include other predictors if they support your model.')
b('Fit your final model using only games completed through Week 8 of the 2026 season. State how your source defines weeks and how you handle Week 0, missing values, canceled games, and teams with limited data.')
b('Reserve Week 9 outcomes for evaluation. Use only information available at the end of Week 8 for model selection, predictor construction, rankings, and predictions. If you use earlier seasons to inform priors, explain how you account for changes between seasons.')
h('Bayesian modeling')
p('Specify and justify your likelihood, team-strength parameters, prior distributions, and any additional effects. Suitable approaches include Bayesian paired-comparison models, models for point margins, and models for team scores with offensive and defensive strengths. Explain how your chosen model yields a comparable measure of overall team strength and a probability of winning.')
b('Account for home-field advantage and neutral-site games. You may use a common home-field effect or a hierarchical model for team-specific effects.')
b('Explain how you identify the team-strength scale, such as by centering strengths or choosing a reference team. If your model has multiple strength components, define how you combine or compare them for ranking.')
b('Fit the model in Stan through RStan or another appropriate Bayesian tool in R. Report sampling settings and assess convergence and sampling quality using appropriate diagnostics, such as R-hat, effective sample sizes, and divergent transitions when applicable.')
b('Use posterior predictive checks to assess model fit. Examine sensitivity to at least one reasonable alternative prior or model specification, using only training data or validation splits within Weeks 1–8.')

d.add_page_break()
h('Team rankings and strength of schedule')
b('Rank teams using a clearly defined posterior summary of strength. Report the top 25 and bottom 25 teams in the combined FBS and FCS pool, with strength estimates and 90% credible intervals. Explain what the scale means and discuss uncertainty in the rankings.')
b('Estimate home-field advantage, report its uncertainty, and interpret its practical effect on a matchup. Explain how home-field advantage enters your model.')
b('Compute each team’s strength of schedule through Week 8 using its opponents’ estimated strengths. Define your calculation, explain how repeated opponents are weighted, and summarize uncertainty using posterior draws. Include the complete results in an appendix or downloadable table.')
h('Week 9 predictions')
p('For every Week 9 game involving an FBS or FCS team in your analysis, report the posterior predictive probability that the designated home team wins and a 90% credible interval for the underlying win probability. Treat neutral-site games accordingly. The interval describes uncertainty about the win probability; it is not an interval for the binary game outcome.')
p('Predictive performance will be evaluated using the sum of log posterior predictive probabilities assigned to the observed Week 9 outcomes. For each game, average the probability of its observed outcome across posterior draws, take the logarithm, and then sum across games. For a home win, use the predicted home-win probability; for a home loss, use one minus that probability. Higher scores indicate better predictions. Use this same win–loss scoring rule regardless of your model’s likelihood, and do not refit or tune the model using Week 9 results.')
h('Rice season simulation')
p('Using the posterior distribution based on games through Week 8, simulate the remaining games in Rice’s 2026 regular-season schedule. Combine simulated results with Rice’s actual record through Week 8 to estimate the probability that Rice finishes with a regular-season winning percentage strictly above 50%. Exclude postseason games and state how you handle any schedule changes.')
p('For each simulated season, draw one set of model parameters from the posterior and use it to simulate all remaining games. Repeat across many posterior draws so your estimate reflects uncertainty in team strengths and randomness in game outcomes. Report the estimated probability and describe your simulation procedure.')
h('Submission and grading')
p('Submit an R Quarto file (.qmd) and its rendered HTML to Canvas. Include your data preparation, model specification, model code, diagnostics, rankings, predictions, and Rice simulation. Use concise explanations and clearly commented code. Cite data sources and provide the steps needed to reproduce your analysis.')
p('The written analysis and code account for 80% of the project grade. If model fitting makes rendering impractical, save fitted results and load them during rendering. You may display slow-fitting code with eval: false, but include the saved summaries, diagnostics, and figures needed to evaluate your work.')
p('Prepare a 10-minute presentation explaining your model, main findings, predictive performance, and limitations. The presentation accounts for 20% of the grade. Submission and presentation dates will be announced in class or on Canvas.')
p('Groups begin with a base score of 100 points upon successfully completing all project requirements. The group with the highest Week 9 predictive score receives 5 bonus points.')

d.core_properties.title = 'Project 2 Bayesian Models of College Football Team Strength in 2026'
d.core_properties.subject = 'SMGT 431 project assignment'
for root in [d.styles.element, d.element]:
    for border in list(root.xpath('.//w:pBdr')):
        border.getparent().remove(border)
d.save(r'C:\Users\gongh\Desktop\SGMT431\Project 2 Bayesian Team Strength 2026.docx')
