module PlayGround.P6_XmlParsing exposing (..)

--import PlayGround.P6_XmlParsing exposing (..)

import Xml.Decode
import Xml.Decode exposing (requiredPath)

type alias AnimeData = 
    { info_id : List String 
    , name : String
    , info : List String}


myDecoder = 
    Xml.Decode.map (\x -> {id = x})
        (Xml.Decode.path ["anime"] (Xml.Decode.single (Xml.Decode.stringAttr "id")))

animeDecoder = 
    Xml.Decode.succeed AnimeData
        |> requiredPath ["anime", "info"] (Xml.Decode.list (Xml.Decode.stringAttr "gid"))
        |> requiredPath ["anime"] (Xml.Decode.single (Xml.Decode.stringAttr "name"))
        |> Xml.Decode.optionalPath ["anime", "info"] (Xml.Decode.list Xml.Decode.string) ["empty"]

a = Xml.Decode.path ["anime", "info"]

decoding = Xml.Decode.decodeString myDecoder xmlTest
decoding2 = Xml.Decode.decodeString animeDecoder xmlTest


xmlTest = 
    """
    <ann>
    <anime id="4658" gid="1097470093" type="TV" name="Jinki:Extend" precision="TV" generated-on="2021-12-09T01:25:00Z">
    <related-prev rel="adapted from" id="4199"/>
    <info gid="1693214504" type="Picture" src="https://cdn.animenewsnetwork.com/thumbnails/fit200x200/encyc/A4658-7.jpg" width="178" height="200">
    <img src="https://cdn.animenewsnetwork.com/thumbnails/fit200x200/encyc/A4658-7.jpg" width="178" height="200"/>
    </info>
    <info gid="2849059477" type="Main title" lang="JA">Jinki:Extend</info>
    <info gid="2660569749" type="Alternative title" lang="RU">Боевые роботы Дзинки</info>
    <info gid="1675008096" type="Alternative title" lang="JA">ジンキ・エクステンド</info>
    <info gid="2445949786" type="Genres">drama</info>
    <info gid="1267102403" type="Genres">science fiction</info>
    <info gid="253304187" type="Themes">Giant Robots</info>
    <info gid="1769392376" type="Themes">mecha</info>
    <info gid="936033876" type="Themes">military</info>
    <info gid="2421355715" type="Themes">real robot</info>
    <info gid="2605299806" type="Objectionable content">TA</info>
    <info gid="3324519232" type="Plot Summary">
    Aoba is a young girl who loves to build models of robots. She lived alone with her grandmother until her grandmother passes away. Shortly after she is kidnapped and brought to a secret base where she discovers a huge robot. The piloted robots fight against Ancient-Jinki in The Grand Savanna, but the true meaning behind the fights is hidden. Aoba works hard at the base so one day she can pilot one of the robots and discover these secrets.
    </info>
    <info gid="2642397968" type="Number of episodes">12</info>
    <info gid="3079748225" type="Vintage">2005-01-05 to 2005-03-23</info>
    <info gid="2071198001" type="Opening Theme">"FLY AWAY" by unicorn table</info>
    <info gid="3833528025" type="Ending Theme">
    "未来とゆう名の答え (<i>Mirai to yuu na no kotae</i>)" by angela
    </info>
    <info gid="3603908636" type="Official website" lang="EN" href="https://web.archive.org/web/20060822152703/http://www25.advfilms.com/titles/extend/index_content.html">ADV - Jinki : Extend Offical Site (English)</info>
    <info gid="2164362191" type="Official website" lang="EN" href="https://www.funimation.com/shows/jinki-extend/">FUNimation's Official Jinki:Extend Website</info>
    <info gid="2344820708" type="Official website" lang="JA" href="http://www.tv-asahi.co.jp/jinki/">TV Asahi's Official "Jinki: Extend" Website</info>
    <info gid="1291886456" type="Official website" lang="JA" href="http://www.jinki.info">ジンキ・エクステンド</info>
    <ratings nb_votes="415" weighted_score="5.9528" bayesian_score="5.99615"/>
    <episode num="1">
    <title gid="3095466740" lang="EN">The Battlefield the Girl Saw</title>
    </episode>
    <episode num="2">
    <title gid="1019682570" lang="EN">The Trail of Tears</title>
    </episode>
    <episode num="3">
    <title gid="2879327918" lang="EN">Quality and Quantity</title>
    </episode>
    <episode num="4">
    <title gid="1042118304" lang="EN">Encounter</title>
    </episode>
    <episode num="5">
    <title gid="1323093475" lang="EN">Foes and Friends</title>
    </episode>
    <episode num="6">
    <title gid="3023688674" lang="EN">The Black Operator</title>
    </episode>
    <episode num="7">
    <title gid="2322922310" lang="EN">Fulfilled Ambition</title>
    </episode>
    <episode num="8">
    <title gid="1400916808" lang="EN">The Silver-winged Visitor</title>
    </episode>
    <episode num="9">
    <title gid="2204235069" lang="EN">The Game's Winner</title>
    </episode>
    <episode num="10">
    <title gid="2038411104" lang="EN">Red and Black</title>
    </episode>
    <episode num="11">
    <title gid="250690944" lang="EN">Family</title>
    </episode>
    <episode num="12">
    <title gid="1897449103" lang="EN">Blue and Red</title>
    </episode>
    <episode num="13">
    <title gid="1997989660" lang="EN">And Then</title>
    </episode>
    <review href="https://www.animenewsnetwork.com/review/jinki-extend/dvd-3">Jinki:Extend DVD 3</review>
    <review href="https://www.animenewsnetwork.com/review/jinki-extend/dvd-2">Jinki:Extend DVD 2</review>
    <review href="https://www.animenewsnetwork.com/review/jinki-extend-1">Jinki:Extend 1</review>
    <release date="2010-10-05" href="https://www.animenewsnetwork.com/encyclopedia/releases.php?id=17679">
    Jinki:Extend - The Complete Series [S.A.V.E. Edition] (DVD)
    </release>
    <release date="2009-09-08" href="https://www.animenewsnetwork.com/encyclopedia/releases.php?id=14829">Jinki:Extend - The Complete Series (DVD)</release>
    <release date="2006-09-05" href="https://www.animenewsnetwork.com/encyclopedia/releases.php?id=7626">Jinki:Extend (DVD 1)</release>
    <release date="2006-11-07" href="https://www.animenewsnetwork.com/encyclopedia/releases.php?id=7552">Jinki:Extend (DVD 2)</release>
    <release date="2007-01-02" href="https://www.animenewsnetwork.com/encyclopedia/releases.php?id=7874">Jinki:Extend (DVD 3)</release>
    <release date="2008-01-01" href="https://www.animenewsnetwork.com/encyclopedia/releases.php?id=10390">
    Jinki:Extend - Complete Collection [Thinpak] (DVD 1-3)
    </release>
    <news datetime="2007-08-10T15:58:20Z" href="https://www.animenewsnetwork.com/news/2007-08-10/unicorn-table-to-perform-at-new-york-anime-festival">
    Unicorn Table to Perform at New York Anime Festival
    </news>
    <news datetime="2007-09-13T13:15:35Z" href="https://www.animenewsnetwork.com/news/2007-09-13/uk's-propeller-tv-expands-anime-network-to-daily-block">
    UK's Propeller TV Expands Anime Network to Daily Block
    </news>
    <news datetime="2007-09-18T13:43:40Z" href="https://www.animenewsnetwork.com/news/2007-09-18/virginia's-akiba-fest-ball-hosts-salia-psychic-lover">
    Virginia's Akiba Fest Ball Hosts Salia, Psychic Lover
    </news>
    <news datetime="2008-01-22T18:55:16Z" href="https://www.animenewsnetwork.com/news/2008-01-22/adv-films-uk-switches-from-us-run-office-to-uk-partner">
    ADV Films UK Switches from US-Run Office to UK Partner
    </news>
    <news datetime="2008-01-30T19:45:30Z" href="https://www.animenewsnetwork.com/news/2008-01-30/adv-films-removes-titles-from-website-update">ADV Films Removes Titles from Website - Update</news>
    <news datetime="2008-02-20T01:34:59Z" href="https://www.animenewsnetwork.com/news/2008-02-19/unicorn-table-to-play-at-western-new-york-tora-con">
    Unicorn Table to Play at Western New York's Tora-Con
    </news>
    <news datetime="2008-06-11T17:57:28Z" href="https://www.animenewsnetwork.com/news/2008-06-11/shonen-gangan-details-gainax-shikabane-hime-aka-anime">
    <cite>Shonen Gangan</cite> Details Gainax's <cite>Shikabane Hime: Aka</cite> Anime
    </news>
    <news datetime="2008-07-04T13:01:00Z" href="https://www.animenewsnetwork.com/news/2008-07-04/funimation-picks-up-over-30-former-ad-vision-titles">
    Funimation Picks Up Over 30 Former AD Vision Titles
    </news>
    <news datetime="2010-02-09T20:38:08Z" href="https://www.animenewsnetwork.com/news/2010-02-09/dubbed-strike-witches-wallflower-halo-clip-posted">
    Dubbed <cite>Strike Witches, Wallflower, Halo</cite> Clip Posted
    </news>
    <news datetime="2010-02-18T03:16:26Z" href="https://www.animenewsnetwork.com/news/2010-02-17/crunchyroll-funimation-stream-shinkai-works-moeyo-ken">
    Crunchyroll, Funimation Stream Shinkai Works, <cite>Moeyo Ken</cite>
    </news>
    <news datetime="2010-03-04T12:00:00Z" href="https://www.animenewsnetwork.com/news/2010-03-04/shattered-angels-gunslinger-girl-ova-more-streamed">
    <cite>Shattered Angels, Gunslinger Girl</cite> OVA, More Streamed
    </news>
    <news datetime="2010-10-05T14:07:52Z" href="https://www.animenewsnetwork.com/news/2010-10-05/north-american-anime-manga-releases-october-3-9">North American Anime, Manga Releases, October 3-9</news>
    <news datetime="2011-02-23T22:38:44Z" href="https://www.animenewsnetwork.com/news/2011-02-23/unicorn-table-singers-to-perform-at-ayacon-warwick">
    Unicorn Table singers to perform at Ayacon, Warwick
    </news>
    <news datetime="2011-06-18T03:57:00Z" href="https://www.animenewsnetwork.com/news/2011-06-17/north-american-stream-list/june-10-17">North American Stream List: June 10-17</news>
    <news datetime="2012-01-30T22:00:00Z" href="https://www.animenewsnetwork.com/news/2012-01-30/adv-court-documents-reveal-amounts-paid-for-29-anime-titles">
    ADV Court Documents Reveal Amounts Paid for 29 Anime Titles
    </news>
    <staff gid="1560974500">
    <task>Director</task>
    <person id="3610">Masahiko Murata</person>
    </staff>
    <staff gid="2111742793">
    <task>Script</task>
    <person id="668">Hiroyuki Kawasaki</person>
    </staff>
    <staff gid="2939729004">
    <task>Script</task>
    <person id="3086">Naruhisa Arakawa</person>
    </staff>
    <staff gid="2553309728">
    <task>Script</task>
    <person id="68814">Hitomi Amamiya</person>
    </staff>
    <staff gid="1759898295">
    <task>Storyboard</task>
    <person id="4422">Kinji Yoshimoto</person>
    </staff>
    <staff gid="1801311718">
    <task>Storyboard</task>
    <person id="8476">Satoshi Saga</person>
    </staff>
    <staff gid="2219649140">
    <task>Episode Director</task>
    <person id="8476">Satoshi Saga</person>
    </staff>
    <staff gid="714854657">
    <task>Music</task>
    <person id="140">Kenji Kawai</person>
    </staff>
    <staff gid="1082163268">
    <task>Original Manga</task>
    <person id="27743">Sirou Tunasima</person>
    </staff>
    <staff gid="2230005074">
    <task>Character Design</task>
    <person id="15209">Naoto Hosoda</person>
    </staff>
    <staff gid="1938405310">
    <task>Art Director</task>
    <person id="7353">Naoko Kosakabe</person>
    </staff>
    <staff gid="1701314492">
    <task>Animation Director</task>
    <person id="4240">Masami Obari</person>
    </staff>
    <staff gid="1515756879">
    <task>Animation Director</task>
    <person id="15209">Naoto Hosoda</person>
    </staff>
    <staff gid="3250390369">
    <task>Animation Director</task>
    <person id="66640">Hiroki Mutaguchi</person>
    </staff>
    <staff gid="3076790874">
    <task>Mechanical design</task>
    <person id="17992">Katsuyuki Tamura</person>
    </staff>
    <staff gid="2676225977">
    <task>Sound Director</task>
    <person id="424">Kazuhiro Wakabayashi</person>
    </staff>
    <staff gid="1843996024">
    <task>Director of Photography</task>
    <person id="7454">Yasuhisa Kondo</person>
    </staff>
    <staff gid="3311864319">
    <task>Producer</task>
    <person id="46074">Hedwig Schleck</person>
    </staff>
    <staff gid="841413005">
    <task>Producer</task>
    <person id="48371">Makoto Takigasaki</person>
    </staff>
    <staff gid="2405542094">
    <task>Producer</task>
    <person id="55498">Shigeru Tateishi</person>
    </staff>
    <cast gid="2661476283" lang="EN">
    <role>Mel J</role>
    <person id="1062">Christine Auten</person>
    </cast>
    <cast gid="2408482882" lang="EN">
    <role>Ryohei Ogawara</role>
    <person id="1064">Jason Douglas</person>
    </cast>
    <cast gid="3934354050" lang="EN">
    <role>Shizuka Tsuzaki</role>
    <person id="2074">Monica Rial</person>
    </cast>
    <cast gid="2680047095" lang="EN">
    <role>Hiroshi Kawamoto</role>
    <person id="2517">Chris Patton</person>
    </cast>
    <cast gid="2380683277" lang="EN">
    <role>Genta Ogawara</role>
    <person id="2525">John Swasey</person>
    </cast>
    <cast gid="1654830201" lang="EN">
    <role>Elnie Tachibana</role>
    <person id="3516">Cynthia Martinez</person>
    </cast>
    <cast gid="3853972106" lang="EN">
    <role>Shiva</role>
    <person id="3660">Kira Vincent-Davis</person>
    </cast>
    <cast gid="1910080947" lang="EN">
    <role>Minami Kosaka</role>
    <person id="4919">Shelley Calene-Black</person>
    </cast>
    <cast gid="3531999063" lang="EN">
    <role>Akao Hiiragi</role>
    <person id="7798">Jessica Boone</person>
    </cast>
    <cast gid="2391728390" lang="EN">
    <role>Rui Kosaka</role>
    <person id="12493">Luci Christian</person>
    </cast>
    <cast gid="1409390351" lang="EN">
    <role>Satsuki Kawamoto</role>
    <person id="13929">Allison Sumrall</person>
    </cast>
    <cast gid="3131068072" lang="EN">
    <role>Hideo Koyatani</role>
    <person id="14187">Nomed Kaerf</person>
    </cast>
    <cast gid="2876815189" lang="EN">
    <role>Yasuyuki Nishihara</role>
    <person id="37623">Blake Shepard</person>
    </cast>
    <cast gid="1384221282" lang="EN">
    <role>Aoba Tsuzaki</role>
    <person id="44595">Brittney Karbowski</person>
    </cast>
    <cast gid="1792866004" lang="JA">
    <role>Shizuka Tsuzaki</role>
    <person id="278">Satsuki Yukino</person>
    </cast>
    <cast gid="2156350370" lang="JA">
    <role>Genta Ogawara</role>
    <person id="458">Rokurō Naya</person>
    </cast>
    <cast gid="3448852547" lang="JA">
    <role>Rui Kōsaka</role>
    <person id="524">Yukari Tamura</person>
    </cast>
    <cast gid="1756973665" lang="JA">
    <role>Elnie Tachibana</role>
    <person id="526">Tomoko Kawakami</person>
    </cast>
    <cast gid="1626578526" lang="JA">
    <role>Aoba Tsuzaki</role>
    <person id="912">Fumiko Orikasa</person>
    </cast>
    <cast gid="1410696507" lang="JA">
    <role>Minami Kōsaka</role>
    <person id="1588">Yoshino Takamori</person>
    </cast>
    <cast gid="3084421961" lang="JA">
    <role>Ryouhei Ogawara</role>
    <person id="7629">Takuma Takewaka</person>
    </cast>
    <cast gid="3270154068" lang="JA">
    <role>Mel J Vanette</role>
    <person id="9961">Junko Minagawa</person>
    </cast>
    <cast gid="3151376498" lang="JA">
    <role>Satsuki Kawamoto</role>
    <person id="13321">Ai Nonaka</person>
    </cast>
    <cast gid="2690394696" lang="JA">
    <role>Akao Hiiragi</role>
    <person id="37191">Yuuna Inamura</person>
    </cast>
    <cast gid="1216199898" lang="TL">
    <role>Genta Ogawara</role>
    <person id="38935">Louie Paraboles</person>
    </cast>
    <cast gid="3162120279" lang="TL">
    <role>Ryouhei Ogawara</role>
    <person id="38935">Louie Paraboles</person>
    </cast>
    <cast gid="1449215999" lang="TL">
    <role>Aoba Tsuzaki</role>
    <person id="39024">Grace Cornel</person>
    </cast>
    <cast gid="2741462152" lang="TL">
    <role>Minami Kosaka</role>
    <person id="39024">Grace Cornel</person>
    </cast>
    <cast gid="867204356" lang="TL">
    <role>Akao Hiiragi</role>
    <person id="39656">Sherwin Revestir</person>
    </cast>
    <cast gid="533292258" lang="TL">
    <role>Shizuka Tsuzaki</role>
    <person id="39656">Sherwin Revestir</person>
    </cast>
    <cast gid="2304322786" lang="TL">
    <role>Rui Kosaka</role>
    <person id="42183">Justeen Niebres</person>
    </cast>
    <cast gid="1655325149" lang="TL">
    <role>Shiva</role>
    <person id="42183">Justeen Niebres</person>
    </cast>
    <credit gid="2489952245">
    <task>Animation Production</task>
    <company id="8883">feel.</company>
    </credit>
    <credit gid="2919332591">
    <task>Production</task>
    <company id="160">TV Asahi</company>
    </credit>
    <credit gid="1743771520">
    <task>Production</task>
    <company id="1850">GANSIS</company>
    </credit>
    <credit gid="2207693755">
    <task>Production</task>
    <company id="8883">feel.</company>
    </credit>
    </anime>
    </ann>
    """