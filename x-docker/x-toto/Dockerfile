FROM python:3.4-alpine
WORKDIR /codea
RUN mkdir /codea/docker-out
ADD . /codea
WORKDIR /codea
RUN pip install -r requirements.txt
CMD ["python", "a.py"]