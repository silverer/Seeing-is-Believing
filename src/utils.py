import requests
from bs4 import BeautifulSoup
import pandas as pd
from os.path import basename
import regex as re
import string
import numpy as np
import math
import data_io
import datetime as dt

def process_request(url):
    r = requests.get(url)
    if r.status_code != 200:
        return 0
    soup = BeautifulSoup(r.content, 'html.parser')
    return soup

FORMATTED = pd.read_csv(f'{data_io.DATA}news_article_format_template.csv')

def format_spreadsheet(df):
    fmtd = pd.concat([FORMATTED, df])
    return fmtd

ARTICLE_LINK_BASE = pd.DataFrame(columns = ['article_title', 
									'article_link', 'article_date'], 
									index = [0])
#ARTICLE_LINK_BASE.loc[0, :] = 'NA'

IMAGE_SUFFIX = ['.jpg','.JPEG','.JPG', '.jpeg', '.png','.PNG', '.gif',
                '.GIF','.tiff', '.mp4', '.MP4']

IMAGE_COLS = ['image_links',
              'image_alt_text',
             'image_captions',
             'image_location']

ARTICLE_TEXT_COLS = ['article_id','gender_first_researcher', 'num_women_mentioned',
                     'num_men_mentioned', 'num_others_mentioned', 'rater_initials',
                     'comments']

ARTICLE_CONTAINER = pd.DataFrame(columns = ['article_id','article_title','article_link',
                        'image_links',
                        'image_alt_text',
                        'article_text',
                        'article_date',
                        'image_captions'], index = [0])

SAVE_PICS = True
def save_photo(this_link, prefix=None):
    if prefix:
        fname = data_io.PHOTO_LOC+prefix+basename(this_link)
    else:
        fname = data_io.PHOTO_LOC+basename(this_link)
    if any(this_link.endswith(s) for s in IMAGE_SUFFIX) == False:
        fname = fname + '.jpeg'
    
    if this_link.startswith("/"):
        print('Could not retrieve photo: ', this_link)
        return 'FAILED'
    try:
        requests.get(this_link).content
    except:
        print(requests.get(this_link).content)
        print('Could not retrieve photo: ', this_link)
        return 'FAILED'
    if SAVE_PICS:
        with open(fname, "wb") as f:
            f.write(requests.get(this_link).content)
    else:
        requests.get(this_link).content
    
    return fname


def check_image_src(img_src, image_url_stem = ''):
    img_src = img_src.strip()
    if img_src.startswith("../../../../"):
        img_src = img_src.replace("../../../../", "")
        
    if image_url_stem != '' and '.com/' not in img_src and\
    '.edu/' not in img_src and '.org/' not in img_src and '.net/' not in img_src:
        #check for double slashes where there shouldn't be one
        if image_url_stem.endswith("/") and img_src.startswith("/"):
            this_link = image_url_stem[:-1] + img_src
        else:
            this_link = image_url_stem + img_src
        
    else:
        if 'http://' not in img_src and 'https://' not in img_src:
            img_src = img_src.strip()
            if image_url_stem == '':
                if img_src[0] == '/':
                    this_link = 'http:/' + img_src
                else:
                    this_link = 'http://' + img_src
            else:
                #check for double slashes where there shouldn't be one
                if image_url_stem.endswith("/") and img_src.startswith("/"):
                    this_link = image_url_stem[:-1] + img_src
                else:
                    this_link = image_url_stem + img_src
        else:
            this_link = img_src
    if any(this_link.endswith(s) for s in IMAGE_SUFFIX):
        this_link = this_link
    else:
        for s in IMAGE_SUFFIX:
            #Sometimes there's extra stuff at the end of filenames--filter that stuff out
            if re.search(s, this_link):
                this_link = this_link[0:re.search(s, this_link).end()]
    
    return(this_link)




def extract_fname(full_fname):
    if type(full_fname)!= str:
        return
    return full_fname.replace(data_io.PHOTO_LOC, '')

def get_link_prefixes(link, school):
    
    pref_end = re.search(f'[.]{school}', link).start()
    pref = link[0:pref_end]
    return pref

def get_embedded_youtube_thumbnail(raw_image_src):
    #re.search("om[/]v[/]", vid_url).end()
    if re.search("embed/", raw_image_src):
        vid_id_start = re.search("embed/", raw_image_src).end()
    else:
        vid_id_start = re.search("om/v/", raw_image_src).end()
        
    if re.search("[?]feature=", raw_image_src):
        vid_id_end = re.search("[?]feature=", raw_image_src).start()
    elif re.search("[?]w", raw_image_src):
        vid_id_end = re.search("[?]w", raw_image_src).start()
    elif re.search("&fs", raw_image_src):
        vid_id_end = re.search("&fs", raw_image_src).start()
    elif raw_image_src.endswith("/"):
        vid_id_end = len(raw_image_src)-1
    else:
        vid_id_end = len(raw_image_src)
    
    vid_id = raw_image_src[vid_id_start:vid_id_end]
    image_link = f"http://img.youtube.com/vi/{vid_id}/maxresdefault.jpg"
    
    if requests.get(image_link).status_code != 200:
        image_link = f"https://img.youtube.com/vi/{vid_id}/hqdefault.jpg"
    return image_link, vid_id


def get_embedded_vimeo_thumbnail(raw_image_src):
    #image_link = "http://i.vimeocdn.com/video/798775866_640.png"
    #https://i.vimeocdn.com/video/798775866.webp?mw=1500&mh=844&q=70
    image_link = re.search("https:\/\/i[.]vimeocdn[.]com\/video\/\d{1,30}[.](jpg|png)", ph).group()
                
    vid_id = re.search("\/\d{1,30}[.]", image_link).group()
    vid_id = vid_id.replace("/", "").replace(".", "")
    
    #vid_id_start = re.search("/video/", raw_image_src).end()
    #vid_id_end = re.search("[.]webp?", raw_image_src).start()
    #vid_id = raw_image_src[vid_id_start:vid_id_end]
    image_link = f"http://i.vimeocdn.com/video/{vid_id}_640.png"
    return image_link, vid_id

def detect_redirect(url, school):
    r = requests.get(url)
    if school not in r.url:
        return r.url
    else:
        return 1
    
def check_article_link(article_link):
    if len(re.findall("https://", article_link)) > 1:
        return False
    else:
        return True
    
def process_article_link_dataset(article_df_orig, uni = 'UNIVERSITY_NAME', 
                                drop_duplicates = True, scramble = True,
                                link_pref = None):
    article_df = article_df_orig.copy()
    if drop_duplicates:
        article_df = article_df.drop_duplicates(subset = ['article_link'])
        article_df = article_df.drop_duplicates(subset = ['article_title'])
        article_df = article_df.reset_index(drop = True)
    article_df['tmp'] = article_df.index.astype(str).to_list()
    article_df['article_id'] = uni + article_df['tmp']
    article_df = article_df.drop(columns = ['tmp'])   
    if scramble:
        article_df = article_df.sample(frac = 1)
        article_df = article_df.reset_index(drop = True)
    if link_pref:
        article_df['link_prefixes'] = article_df.apply(lambda x: get_link_prefixes(x['article_link'],
                                                                            link_pref),
                                                            axis=1)
    #
    return article_df
    

def clean_save_dataset(df, whole_filename, cleaned_filename, 
                  return_type = 'whole',
                  sample_size = None):
    
    df['photo_filename'] = df['image_location'].apply(extract_fname)

    fmtd_df = format_spreadsheet(df)
    if data_io.DATA in whole_filename or data_io.GDRIVE in whole_filename:
        writer = pd.ExcelWriter(f'{whole_filename}', engine='xlsxwriter')
    else:
        writer = pd.ExcelWriter(f"{data_io.DATA}{whole_filename}", engine = 'xlsxwriter')
    
    fmtd_df.to_excel(writer, index=False,
                     encoding='utf-8-sig')
    writer.close()
    if sample_size:
        if len(df) > sample_size:
            keep_ids = df.dropna(subset=['article_id'])
            keep_ids = df.drop_duplicates(subset=['article_id'], keep='first')
            keep_ids = keep_ids.sample(sample_size)
            subset = df[df['article_id'].isin(keep_ids['article_id'].to_list())]
            if data_io.DATA in cleaned_filename or data_io.GDRIVE in cleaned_filename:
                writer = pd.ExcelWriter(f'{cleaned_filename}', engine='xlsxwriter')
            else:
                writer = pd.ExcelWriter(f"{data_io.DATA}{cleaned_filename}", engine = 'xlsxwriter')
                
            fmtd_subset = format_spreadsheet(subset)
            fmtd_subset.to_excel(writer, index=False,
                             encoding='utf-8-sig')
            writer.close()
    if return_type=='whole':
        return df
    else:
        return subset

def process_image(image, prefix, article_container, idx, 
                  src_key = 'src', img_stem = '',
                 image_type = 'image', get_src = True):
    if image_type == 'youtube':
        image_link, vid_id = get_embedded_youtube_thumbnail(image)
        prefix = f"{prefix}_{vid_id}"
        alt_text = 'NO ALT TEXT'
    elif image_type == 'vimeo':
        image_link, vid_id = get_embedded_vimeo_thumbnail(image)
        prefix = f"{prefix}_{vid_id}"
        alt_text = 'NO ALT TEXT'
    else:
        if type(src_key) != str:
            for s in src_key:
                if image.has_attr(s):
                    src_key = s
                    break
        if get_src:
            image_link = check_image_src(image[src_key], image_url_stem = img_stem)
        else:
            image_link = check_image_src(image, image_url_stem = img_stem)
        try:
            alt_text = image['alt']
        except:
            alt_text = 'NO ALT TEXT'
    try:
        article_container.loc[idx, 'image_location'] = save_photo(image_link.strip(), prefix=prefix)
    except:
        article_container.loc[idx, 'image_location'] = 'FAILED'
    article_container.loc[idx, 'image_alt_text'] = alt_text
    article_container.loc[idx, 'image_links'] = image_link.strip()
    return article_container

def setup_article_container(link = 'art_link', title = 'art_title',
                            date = 'art_date', text = 'art_text',
                           ref_links = None):
    article_container = ARTICLE_CONTAINER.copy()
    article_container.loc[0, 'article_link'] = link
    article_container.loc[0, 'article_title'] = title
    article_container.loc[0, 'article_date'] = date
    article_container.loc[0, 'article_text'] = text
    if ref_links:
        article_container['ref_links'] = None
        article_container['ref_links'] = article_container['ref_links'].astype('object')
        article_container.at[0, 'ref_links']=ref_links
    return article_container

def remove_captions(caption_list, art_text):
    for c in caption_list:
        if type(c) == str:
            if c in art_text:
                art_text = art_text.replace(c, " ")
    return art_text
#treat_as_one indicates whether .get_text() and .find_all("a") should be called on art_text directly
def process_article_text(art_text, get_links = False, treat_as_one = False):
    all_links = []
    if type(art_text) == str:
        article_text = art_text.strip().replace("\xa0", " ")
    
    else:
        if treat_as_one:
            article_text = art_text.get_text().strip().replace("\xa0", " ")
            all_links = [l['href'] for l in art_text.find_all("a") if l.has_attr('href') and \
                         (l['href'].startswith("http") or l['href'].startswith("www") or l['href'].startswith("../"))]
        elif type(art_text) == list:
            article_text = [a.get_text().strip().replace("\xa0", " ") for a in art_text]
            article_text = ' '.join(article_text)
            for a in art_text:
                all_links.extend([l['href'] for l in a.find_all("a") if l.has_attr('href') and \
                                  (l['href'].startswith("http") or l['href'].startswith("www") or \
                                   l['href'].startswith("../"))])
        else:
            if len(art_text)> 1:
                article_text = [a.get_text().strip().replace("\xa0", " ") for a in art_text]
                article_text = ' '.join(article_text)
                for a in art_text:
                    all_links.extend([l['href'] for l in a.find_all("a") if l.has_attr('href') and \
                                      (l['href'].startswith("http") or l['href'].startswith("www") or \
                                       l['href'].startswith("../"))])
            else:
                article_text = art_text.get_text().strip().replace("\xa0", " ")
                all_links = [l['href'] for l in art_text.find_all("a") if l.has_attr('href') and \
                         (l['href'].startswith("http") or l['href'].startswith("www") or l['href'].startswith("../"))]

    article_text = re.sub("\n{1,5}", " ", article_text)
    article_text = re.sub("\t{1,5}", " ", article_text)
    article_text = re.sub("\s{2,10}", " ", article_text)
    if get_links:
        all_links = list(set(all_links))
        return article_text, all_links
    return article_text
