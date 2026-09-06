local lecture_number = "4"
local current_footer = "SGMT 431 · Lecture 4 · Overview"

local function stringify(value)
  return pandoc.utils.stringify(value)
end

local function strip_detail_number(inlines)
  if #inlines == 0 or inlines[1].t ~= "Str" then
    return inlines
  end

  if inlines[1].text:match("^%d+%.%d+%.%d+") then
    table.remove(inlines, 1)
    if #inlines > 0 and inlines[1].t == "Space" then
      table.remove(inlines, 1)
    end
  end

  return inlines
end

local function html_escape(text)
  return text
    :gsub("&", "&amp;")
    :gsub("<", "&lt;")
    :gsub(">", "&gt;")
end

function Header(header)
  if header.level == 2 and header.classes:includes("section-slide") then
    local section_text = stringify(header.content)
    local section_number, section_title = section_text:match("^(%d+%.%d+)%s+(.+)$")

    if section_number then
      current_footer = "SGMT 431 · Lecture " .. section_number .. " · " .. section_title
    else
      current_footer = "SGMT 431 · Lecture " .. lecture_number .. " · " .. section_text
    end
  elseif header.level == 2 or header.level == 3 then
    header.content = strip_detail_number(header.content)
  end

  if header.level == 2 then
    local footer = pandoc.RawBlock(
      "html",
      '<div class="section-footer">' .. html_escape(current_footer) .. "</div>"
    )
    return {header, footer}
  end

  return header
end
